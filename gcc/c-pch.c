/* Precompiled header implementation for the C languages.
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cpplib.h"
#include "tree.h"
#include "flags.h"
#include "c-common.h"
#include "output.h"
#include "toplev.h"
#include "debug.h"
#include "c-pragma.h"
#include "ggc.h"
#include "langhooks.h"

struct c_pch_validity
{
  unsigned char debug_info_type;
};

struct c_pch_header 
{
  unsigned long asm_size;
};

#define IDENT_LENGTH 8

static FILE *pch_outfile;

extern char *asm_file_name;
static long asm_file_startpos;

static const char * get_ident PARAMS((void));

/* Compute an appropriate 8-byte magic number for the PCH file, so that
   utilities like file(1) can identify it, and so that GCC can quickly
   ignore non-PCH files and PCH files that are of a completely different
   format.  */

static const char *
get_ident()
{
  static char result[IDENT_LENGTH];
  static const char template[IDENT_LENGTH] = "gpch.011";
  
  memcpy (result, template, IDENT_LENGTH);
  if (c_language == clk_c)
    result[4] = flag_objc ? 'o' : 'C';
  else if (c_language == clk_cplusplus)
    result[4] = flag_objc ? 'O' : '+';
  else
    abort ();
  return result;
}

/* Prepare to write a PCH file.  This is called at the start of 
   compilation.  */

void
pch_init ()
{
  FILE *f;
  struct c_pch_validity v;
  
  if (! pch_file)
    return;
  
  f = fopen (pch_file, "w+b");
  if (f == NULL)
    fatal_io_error ("can't open %s", pch_file);
  pch_outfile = f;
  
  v.debug_info_type = write_symbols;
  if (fwrite (get_ident(), IDENT_LENGTH, 1, f) != 1
      || fwrite (&v, sizeof (v), 1, f) != 1)
    fatal_io_error ("can't write to %s", pch_file);

  /* We need to be able to re-read the output.  */
  /* The driver always provides a valid -o option.  */
  if (asm_file_name == NULL
      || strcmp (asm_file_name, "-") == 0)
    fatal_error ("`%s' is not a valid output file", asm_file_name);
  
  asm_file_startpos = ftell (asm_out_file);
  
  /* Let the debugging format deal with the PCHness.  */
  (*debug_hooks->handle_pch) (0);
  
  cpp_save_state (parse_in, f);
}

/* Write the PCH file.  This is called at the end of a compilation which
   will produce a PCH file.  */

void
c_common_write_pch ()
{
  char *buf;
  long asm_file_end;
  long written;
  struct c_pch_header h;

  (*debug_hooks->handle_pch) (1);

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

/* Check the PCH file called NAME, open on FD, to see if it can be used
   in this compilation.  */

int
c_common_valid_pch (pfile, name, fd)
     cpp_reader *pfile;
     const char *name;
     int fd;
{
  int sizeread;
  int result;
  char ident[IDENT_LENGTH];
  const char *pch_ident;
  struct c_pch_validity v;

  if (! allow_pch)
    return 2;

  /* Perform a quick test of whether this is a valid
     precompiled header for the current language.  */

  sizeread = read (fd, ident, IDENT_LENGTH);
  if (sizeread == -1)
    {
      fatal_io_error ("can't read %s", name);
      return 2;
    }
  else if (sizeread != IDENT_LENGTH)
    return 2;
  
  pch_ident = get_ident();
  if (memcmp (ident, pch_ident, IDENT_LENGTH) != 0)
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
	    cpp_error (pfile, DL_WARNING, "%s: not for %s", name,
		       lang_hooks.name);
	  else 
	    /* Not any kind of PCH.  */
	    cpp_error (pfile, DL_WARNING, "%s: not a PCH file", name);
	}
      return 2;
    }

  if (read (fd, &v, sizeof (v)) != sizeof (v))
    {
      fatal_io_error ("can't read %s", name);
      return 2;
    }

  /* The allowable debug info combinations are that either the PCH file
     was built with the same as is being used now, or the PCH file was
     built for some kind of debug info but now none is in use.  */
  if (v.debug_info_type != write_symbols
      && write_symbols != NO_DEBUG)
    {
      if (cpp_get_options (pfile)->warn_invalid_pch)
	cpp_error (pfile, DL_WARNING, 
		   "%s: created with -g%s, but used with -g%s", name,
		   debug_type_names[v.debug_info_type],
		   debug_type_names[write_symbols]);
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

/* Load in the PCH file NAME, open on FD.  It was originally searched for
   by ORIG_NAME.  */

void
c_common_read_pch (pfile, name, fd, orig_name)
     cpp_reader *pfile;
     const char *name;
     int fd;
     const char *orig_name ATTRIBUTE_UNUSED;
{
  FILE *f;
  struct c_pch_header h;
  char *buf;
  unsigned long written;
  struct save_macro_data *smd;
  
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
}
