/* Subroutines for GNU compiler for Intel 80x86 running DG/ux
   Copyright (C) 1993, 1995, 1997, 1999 Free Software Foundation, Inc.
   Currently maintained by (gcc@dg-rtp.dg.com)

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

#include <time.h>
#include "i386/i386.c"

struct lang_independent_option
{
  char *string;
  int *variable;
  int on_value;
  char *description;
};

static int
output_option (file, sep, type, name, indent, pos, max)
     FILE *file;
     char *sep;
     char *type;
     char *name;
     char *indent;
     int pos;
     int max;
{
  if (strlen (sep) + strlen (type) + strlen (name) + pos > max)
    {
      fprintf (file, indent);
      return fprintf (file, "%s%s", type, name);
    }
  return pos + fprintf (file, "%s%s%s", sep, type, name);
}

static struct { 
  char *name; 
  int value; 
  const char * description;
} m_options[] = TARGET_SWITCHES;

static void
output_options (file, f_options, f_len, W_options, W_len,
		pos, max, sep, indent, term)
     FILE *file;
     struct lang_independent_option *f_options;
     struct lang_independent_option *W_options;
     int f_len, W_len;
     int pos;
     int max;
     int sep;
     char *indent;
     char *term;
{
  register int j;

  if (optimize)
    pos = output_option (file, sep, "-O", "", indent, pos, max);
  if (write_symbols != NO_DEBUG)
    pos = output_option (file, sep, "-g", "", indent, pos, max);
/*  if (flag_traditional)
    pos = output_option (file, sep, "-traditional", "", indent, pos, max);*/
  if (profile_flag)
    pos = output_option (file, sep, "-p", "", indent, pos, max);
  if (profile_block_flag)
    pos = output_option (file, sep, "-a", "", indent, pos, max);

  for (j = 0; j < f_len; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = output_option (file, sep, "-f", f_options[j].string,
			   indent, pos, max);

  for (j = 0; j < W_len; j++)
    if (*W_options[j].variable == W_options[j].on_value)
      pos = output_option (file, sep, "-W", W_options[j].string,
			   indent, pos, max);

  for (j = 0; j < sizeof m_options / sizeof m_options[0]; j++)
    if (m_options[j].name[0] != '\0'
	&& m_options[j].value > 0
	&& ((m_options[j].value & target_flags)
	    == m_options[j].value))
      pos = output_option (file, sep, "-m", m_options[j].name,
			   indent, pos, max);

  if (ix86_cpu_string)
    pos = output_option (file, sep, "-mcpu=", ix86_cpu_string, 
			 indent, pos, max);
  if (ix86_arch_string)
    pos = output_option (file, sep, "-march=", ix86_arch_string, 
			 indent, pos, max);
  fprintf (file, term);
}

/* Output to FILE the start of the assembler file.  */

void
output_file_start (file, f_options, f_len, W_options, W_len)
     FILE *file;
     struct lang_independent_option *f_options;
     struct lang_independent_option *W_options;
     int f_len, W_len;
{
  register int pos;

  output_file_directive (file, main_input_filename);
  fprintf (file, "\t.version\t\"01.01\"\n");			\
  /* Switch to the data section so that the coffsem symbol and the
     gcc2_compiled. symbol aren't in the text section.  */
  data_section ();

  pos = fprintf (file, "\n// cc1 (%s) arguments:", VERSION_STRING);
  output_options (file, f_options, f_len, W_options, W_len,
		  pos, 75, " ", "\n// ", "\n\n");

#ifdef TARGET_IDENTIFY_REVISION
  if (TARGET_IDENTIFY_REVISION)
    {
      char indent[256];

      time_t now = time ((time_t *)0);
      sprintf (indent, "]\"\n\t%s\t \"@(#)%s [", IDENT_ASM_OP, main_input_filename);
      fprintf (file, indent+3);
      pos = fprintf (file, "gcc %s, %.24s,", VERSION_STRING, ctime (&now));
      output_options (file, f_options, f_len, W_options, W_len,
		      pos, 150 - strlen (indent), " ", indent, "]\"\n\n");
    }
#endif /* TARGET_IDENTIFY_REVISION */
}

#ifndef CROSS_COMPILE
#if defined (_abort_aux) 
/* Debugging aid to be registered via `atexit'.  See the definition
   of abort in dgux.h.  */
void
abort_aux ()
{
  extern int insn_;
  extern char * file_;
  extern int line_;
  static int done;
  rtx line_note;

  if (done++)
    return;
  if (file_ || line_)
    {
      if (write_symbols != NO_DEBUG)
	{
	  for (line_note = (rtx) insn_ ; line_note != 0 ; line_note = PREV_INSN (line_note))
	    if (GET_CODE (line_note) == NOTE && NOTE_LINE_NUMBER (line_note) > 0)
	      break;
	  if (line_note != 0)
	    {
	      error_with_file_and_line (NOTE_SOURCE_FILE (line_note),
					NOTE_LINE_NUMBER (line_note),
					"Internal gcc abort from %s:%d",
					file_ ? file_ : "<nofile>", line_);
	      if (insn_ && file_ && strcmp (file_, "toplev.c"))
		{
		  error_with_file_and_line (NOTE_SOURCE_FILE (line_note),
					    NOTE_LINE_NUMBER (line_note),
					    "The local variable `insn' has the value:", 0);
		  debug_rtx ((rtx) insn_);
		}
	    }
	}
      if (write_symbols == NO_DEBUG || line_note == 0)
	{
	  error ("Internal gcc abort from %s:%d",
		 file_ ? file_ : "<nofile>", line_);
	  if (insn_ && file_ && strcmp (file_, "toplev.c"))
	    {
	      error ("The local variable `insn' has the value:", 0);
	      debug_rtx ((rtx) insn_);
	    }
	}
    }
}
#endif
#endif


