/* Precompiled header implementation for the C languages.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cpplib.h"
#include "tree.h"
#include "c-common.h"
#include "output.h"
#include "toplev.h"
#include "debug.h"
#include "c-pragma.h"
#include "ggc.h"

struct c_pch_header 
{
  unsigned long asm_size;
};

static const char pch_ident[8] = "gpchC010";

static FILE *pch_outfile;

extern char *asm_file_name;
static long asm_file_startpos;

void
pch_init ()
{
  FILE *f;
  
  if (pch_file)
    {
      /* We're precompiling a header file, so when it's actually used,
	 it'll be at least one level deep.  */
      (*debug_hooks->start_source_file) (lineno, input_filename);

      f = fopen (pch_file, "w+b");
      if (f == NULL)
	fatal_io_error ("can't open %s", pch_file);
      pch_outfile = f;
      
      if (fwrite (pch_ident, sizeof (pch_ident), 1, f) != 1)
	fatal_io_error ("can't write to %s", pch_file);

      /* We need to be able to re-read the output.  */
      /* The driver always provides a valid -o option.  */
      if (asm_file_name == NULL
	  || strcmp (asm_file_name, "-") == 0)
	fatal_error ("`%s' is not a valid output file", asm_file_name);

      asm_file_startpos = ftell (asm_out_file);
      
      cpp_save_state (parse_in, f);
    }
}

void
c_common_write_pch ()
{
  char *buf;
  long asm_file_end;
  long written;
  struct c_pch_header h;

  cpp_write_pch_deps (parse_in, pch_outfile);

  asm_file_end = ftell (asm_out_file);
  h.asm_size = asm_file_end - asm_file_startpos;
  
  if (fwrite (&h, sizeof (h), 1, pch_outfile) != 1)
    fatal_io_error ("can't write %s", pch_file);
  
  buf = xmalloc (16384);
  fflush (asm_out_file);

  if (fseek (asm_out_file, asm_file_startpos, SEEK_SET) != 0)
    fatal_io_error ("can't seek in %s", asm_file_name);

  for (written = asm_file_startpos; written < asm_file_end; )
    {
      long size = asm_file_end - written;
      if (size > 16384)
	size = 16384;
      if (fread (buf, size, 1, asm_out_file) != 1)
	fatal_io_error ("can't read %s", asm_file_name);
      if (fwrite (buf, size, 1, pch_outfile) != 1)
	fatal_io_error ("can't write %s", pch_file);
      written += size;
    }
  free (buf);

  gt_pch_save (pch_outfile);
  cpp_write_pch_state (parse_in, pch_outfile);

  fclose (pch_outfile);
}

int
c_common_valid_pch (pfile, name, fd)
     cpp_reader *pfile;
     const char *name;
     int fd;
{
  int sizeread;
  int result;
  char ident[sizeof (pch_ident)];

  if (! allow_pch)
    return 2;

  /* Perform a quick test of whether this is a valid
     precompiled header for C.  */

  sizeread = read (fd, ident, sizeof (pch_ident));
  if (sizeread == -1)
    {
      fatal_io_error ("can't read %s", name);
      return 2;
    }
  else if (sizeread != sizeof (pch_ident))
    return 2;
  
  if (memcmp (ident, pch_ident, sizeof (pch_ident)) != 0)
    {
      if (cpp_get_options (pfile)->warn_invalid_pch)
	{
	  if (memcmp (ident, pch_ident, 5) == 0)
	    /* It's a PCH, for the right language, but has the wrong version.
	     */
	    cpp_error (pfile, DL_WARNING, 
		       "%s: not compatible with this GCC version", name);
	  else if (memcmp (ident, pch_ident, 4) == 0)
	    /* It's a PCH for the wrong language.  */
	    cpp_error (pfile, DL_WARNING, "%s: not for C language", name);
	  else 
	    /* Not any kind of PCH.  */
	    cpp_error (pfile, DL_WARNING, "%s: not a PCH file", name);
	}
      return 2;
    }

  /* Check the preprocessor macros are the same as when the PCH was
     generated.  */
  
  result = cpp_valid_state (pfile, name, fd);
  if (result == -1)
    return 2;
  else
    return result == 0;
}

void
c_common_read_pch (pfile, name, fd, orig_name)
     cpp_reader *pfile;
     const char *name;
     int fd;
     const char *orig_name;
{
  FILE *f;
  struct c_pch_header h;
  char *buf;
  unsigned long written;
  struct save_macro_data *smd;
  
  /* Before we wrote the file, we started a source file, so we have to start
     one here to match.  */
  (*debug_hooks->start_source_file) (lineno, orig_name);
  
  f = fdopen (fd, "rb");
  if (f == NULL)
    {
      cpp_errno (pfile, DL_ERROR, "calling fdopen");
      return;
    }

  allow_pch = 0;

  if (fread (&h, sizeof (h), 1, f) != 1)
    {
      cpp_errno (pfile, DL_ERROR, "reading");
      return;
    }

  buf = xmalloc (16384);
  for (written = 0; written < h.asm_size; )
    {
      long size = h.asm_size - written;
      if (size > 16384)
	size = 16384;
      if (fread (buf, size, 1, f) != 1
	  || fwrite (buf, size, 1, asm_out_file) != 1)
	cpp_errno (pfile, DL_ERROR, "reading");
      written += size;
    }
  free (buf);

  cpp_prepare_state (pfile, &smd);

  gt_pch_restore (f);

  if (cpp_read_state (pfile, name, f, smd) != 0)
    return;

  fclose (f);

  (*debug_hooks->end_source_file) (lineno);
}
