/* LTO IL options.

   Copyright 2009 Free Software Foundation, Inc.
   Contributed by Simon Baldwin <simonb@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "hashtab.h"
#include "ggc.h"
#include "vec.h"
#include "bitmap.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "target.h"
#include "toplev.h"
#include "lto-streamer.h"

/* When a file is initially compiled, the options used when generating
   the IL are not necessarily the same as those used when linking the
   objects into the final executable.  In general, most build systems
   will proceed with something along the lines of:

   	$ gcc <cc-flags> -flto -c f1.c -o f1.o
	$ gcc <cc-flags> -flto -c f2.c -o f2.o
	...
	$ gcc <cc-flags> -flto -c fN.c -o fN.o

   And the final link may or may not include the same <cc-flags> used
   to generate the initial object files:

   	$ gcc <ld-flags> -flto -o prog f1.o ... fN.o

   Since we will be generating final code during the link step, some
   of the flags used during the compile step need to be re-applied
   during the link step.  For instance, flags in the -m family.

   The idea is to save a selected set of <cc-flags> in a special
   section of the initial object files.  This section is then read
   during linking and the options re-applied.

   FIXME lto.  Currently the scheme is limited in that only the
   options saved on the first object file (f1.o) are read back during
   the link step.  This means that the options used to compile f1.o
   will be applied to ALL the object files in the final link step.
   More work needs to be done to implement a merging and validation
   mechanism, as this will not be enough for all cases.  */

/* Saved options hold the type of the option (currently CL_TARGET or
   CL_COMMON), and the code, argument, and value.  */

typedef struct GTY(()) opt_d
{
  unsigned int type;
  size_t code;
  char *arg;
  int value;
} opt_t;

DEF_VEC_O (opt_t);
DEF_VEC_ALLOC_O (opt_t, heap);


/* Options are held in two vectors, one for those registered by
   command line handling code, and the other for those read in from
   any LTO IL input.  */
static VEC(opt_t, heap) *user_options = NULL;
static VEC(opt_t, heap) *file_options = NULL;

/* Iterate FROM in reverse, writing option codes not yet in CODES into *TO.
   Mark each new option code encountered in CODES.  */

static void
reverse_iterate_options (VEC(opt_t, heap) *from, VEC(opt_t, heap) **to,
			 bitmap codes)
{
  int i;

  for (i = VEC_length (opt_t, from); i > 0; i--)
    {
      const opt_t *const o = VEC_index (opt_t, from, i - 1);

      if (bitmap_set_bit (codes, o->code))
	VEC_safe_push (opt_t, heap, *to, o);
    }
}

/* Concatenate options vectors FIRST and SECOND, rationalize so that only the
   final of any given option remains, and return the result.  */

static VEC(opt_t, heap) *
concatenate_options (VEC(opt_t, heap) *first, VEC(opt_t, heap) *second)
{
  VEC(opt_t, heap) *results = NULL;
  bitmap codes = lto_bitmap_alloc ();

  reverse_iterate_options (second, &results, codes);
  reverse_iterate_options (first, &results, codes);

  lto_bitmap_free (codes);
  return results;
}

/* Clear the options vector in *OPTS_P and set it to NULL.  */

static void
clear_options (VEC(opt_t, heap) **opts_p)
{
  int i;
  opt_t *o;

  for (i = 0; VEC_iterate (opt_t, *opts_p, i, o); i++)
    free (o->arg);

  VEC_free (opt_t, heap, *opts_p);
}

/* Write LENGTH bytes from ADDR to STREAM.  */

static void
output_data_stream (struct lto_output_stream *stream,
                    const void *addr, size_t length)
{
  lto_output_data_stream (stream, addr, length);
}

/* Write string STRING to STREAM.  */

static void
output_string_stream (struct lto_output_stream *stream, const char *string)
{
  bool flag = false;

  if (string != NULL)
    {
      const size_t length = strlen (string);

      flag = true;
      output_data_stream (stream, &flag, sizeof (flag));
      output_data_stream (stream, &length, sizeof (length));
      output_data_stream (stream, string, length);
    }
  else
    output_data_stream (stream, &flag, sizeof (flag));
}

/* Read LENGTH bytes from STREAM to ADDR.  */

static void
input_data_block (struct lto_input_block *ib, void *addr, size_t length)
{
  size_t i;
  unsigned char *const buffer = (unsigned char *const) addr;

  for (i = 0; i < length; i++)
    buffer[i] = lto_input_1_unsigned (ib);
}

/* Return a string from IB.  The string is allocated, and the caller is
   responsible for freeing it.  */

static char *
input_string_block (struct lto_input_block *ib)
{
  bool flag;

  input_data_block (ib, &flag, sizeof (flag));
  if (flag)
    {
      size_t length;
      char *string;

      input_data_block (ib, &length, sizeof (length));
      string = (char *) xcalloc (1, length + 1);
      input_data_block (ib, string, length);

      return string;
    }
  else
    return NULL;
}

/* Return true if this option is one we need to save in LTO output files.
   At present, we pass along all target options, and common options that
   involve position independent code.

   TODO This list of options requires expansion and rationalization.
   Among others, optimization options may well be appropriate here.  */

static bool
register_user_option_p (size_t code, int type)
{
  if (type == CL_TARGET)
    return true;
  else if (type == CL_COMMON)
    {
      return (code == OPT_fPIC
	      || code == OPT_fpic
	      || code == OPT_fPIE
	      || code == OPT_fpie
	      || code == OPT_fcommon
	      || code == OPT_fexceptions);
    }

  return false;
}

/* Note command line option with the given TYPE and CODE, ARG, and VALUE.
   If relevant to LTO, save it in the user options vector.  */

void
lto_register_user_option (size_t code, const char *arg, int value, int type)
{
  if (register_user_option_p (code, type))
    {
      opt_t o;

      o.type = type;
      o.code = code;
      if (arg != NULL)
	{
	  o.arg = (char *) xmalloc (strlen (arg) + 1);
	  strcpy (o.arg, arg);
	}
      else
	o.arg = NULL;
      o.value = value;
      VEC_safe_push (opt_t, heap, user_options, &o);
    }
}

/* Empty the saved user options vector.  */

void
lto_clear_user_options (void)
{
  clear_options (&user_options);
}

/* Empty the saved file options vector.  */

void
lto_clear_file_options (void)
{
  clear_options (&file_options);
}

/* Concatenate the user options and any file options read from an LTO IL
   file, and serialize them to STREAM.  File options precede user options
   so that the latter override the former when reissued.  */

static void
output_options (struct lto_output_stream *stream)
{
  VEC(opt_t, heap) *opts = concatenate_options (file_options, user_options);
  const size_t length = VEC_length (opt_t, opts);
  int i;
  opt_t *o;

  output_data_stream (stream, &length, sizeof (length));

  for (i = 0; VEC_iterate (opt_t, opts, i, o); i++)
    {
      output_data_stream (stream, &o->type, sizeof (o->type));
      output_data_stream (stream, &o->code, sizeof (o->code));
      output_string_stream (stream, o->arg);
      output_data_stream (stream, &o->value, sizeof (o->value));
    }

  VEC_free (opt_t, heap, opts);
}

/* Write currently held options to an LTO IL section.  */

void
lto_write_options (void)
{
  char *const section_name = lto_get_section_name (LTO_section_opts, NULL);
  struct lto_output_stream stream;
  struct lto_simple_header header;
  struct lto_output_stream *header_stream;

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  memset (&stream, 0, sizeof (stream));
  output_options (&stream);

  memset (&header, 0, sizeof (header));
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = LTO_section_opts;

  header.compressed_size = 0;
  header.main_size = stream.total_size;

  header_stream = ((struct lto_output_stream *)
		   xcalloc (1, sizeof (*header_stream)));
  lto_output_data_stream (header_stream, &header, sizeof (header));
  lto_write_stream (header_stream);
  free (header_stream);

  lto_write_stream (&stream);
  lto_end_section ();
}

/* Unserialize an options vector from IB, and append to file_options.  */

static void
input_options (struct lto_input_block *ib)
{
  size_t length, i;

  input_data_block (ib, &length, sizeof (length));

  for (i = 0; i < length; i++)
    {
      opt_t o;

      input_data_block (ib, &o.type, sizeof (o.type));
      input_data_block (ib, &o.code, sizeof (o.code));
      o.arg = input_string_block (ib);
      input_data_block (ib, &o.value, sizeof (o.value));
      VEC_safe_push (opt_t, heap, file_options, &o);
    }
}

/* Read options from an LTO IL section.  */

void
lto_read_file_options (struct lto_file_decl_data *file_data)
{
  size_t len;
  const char *data;
  const struct lto_simple_header *header;
  int opts_offset;
  struct lto_input_block ib;

  data = lto_get_section_data (file_data, LTO_section_opts, NULL, &len);
  header = (const struct lto_simple_header *) data;
  opts_offset = sizeof (*header);

  lto_check_version (header->lto_header.major_version,
		     header->lto_header.minor_version);

  LTO_INIT_INPUT_BLOCK (ib, data + opts_offset, 0, header->main_size);
  input_options (&ib);

  lto_free_section_data (file_data, LTO_section_opts, 0, data, len);
}

/* Concatenate the user options and any file options read from an LTO IL
   file, and reissue them as if all had just been read in from the command
   line.  As with serialization, file options precede user options.  */

void
lto_reissue_options (void)
{
  VEC(opt_t, heap) *opts = concatenate_options (file_options, user_options);
  int i;
  opt_t *o;

  for (i = 0; VEC_iterate (opt_t, opts, i, o); i++)
    {
      const struct cl_option *option = &cl_options[o->code];

      if (option->flag_var)
	set_option (option, o->value, o->arg);

      if (o->type == CL_TARGET)
	targetm.handle_option (o->code, o->arg, o->value);
      else if (o->type == CL_COMMON)
	gcc_assert (option->flag_var);
      else
	gcc_unreachable ();
    }

  VEC_free (opt_t, heap, opts);
}
