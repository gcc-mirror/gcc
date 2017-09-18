/* Subroutines for log output for Atmel AVR back end.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (avr@gjlay.de)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.
   
   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "function.h"
#include "rtl.h"
#include "tree.h"
#include "tree-pass.h"	/* for current_pass */
#include "memmodel.h"
#include "tm_p.h"
#include "print-tree.h"

/* This file supplies some functions for AVR back-end developers
   with a printf-like interface.  The functions are called through
   macros `avr_dump', `avr_edump' or `avr_fdump' from avr-protos.h:

   avr_fdump (FILE *stream, const char *fmt, ...);
   avr_edump (fmt, ...) is a shortcut for avr_fdump (stderr, fmt, ...)
   avr_dump (fmt, ...)  is a shortcut for avr_fdump (dump_file, fmt, ...)

  == known %-codes ==

  b: bool
  r: rtx
  t: tree
  T: tree (brief)
  C: enum rtx_code
  m: machine_mode
  R: enum reg_class
  L: insn list
  H: location_t

  == no arguments ==

  A: call abort()
  f: current_function_name()
  F: caller (via __FUNCTION__)
  P: Pass name and number
  ?: Print caller, current function and pass info
  !: Ditto, but only print if in a pass with static pass number,
     else return.

  == same as printf ==

  %: %
  c: char
  s: string
  d: int (decimal)
  x: int (hex)
*/

/* Set according to -mlog= option.  */
avr_log_t avr_log;

/* The worker function implementing the %-codes */
static void avr_log_vadump (FILE*, const char*, va_list);

/* Wrapper for avr_log_vadump.  If STREAM is NULL we are called by avr_dump,
   i.e. output to dump_file if available.  The 2nd argument is __FUNCTION__.
   The 3rd argument is the format string. */

int
avr_vdump (FILE *stream, const char *caller, ...)
{
  va_list ap;
        
  if (NULL == stream && dump_file)
    stream = dump_file;

  va_start (ap, caller);
  if (stream)
    avr_log_vadump (stream, caller, ap);
  va_end (ap);

  return 1;
}


/* Worker function implementing the %-codes and forwarding to
   respective print/dump function.  */

static void
avr_log_vadump (FILE *file, const char *caller, va_list ap)
{
  char bs[3] = {'\\', '?', '\0'};

  /* 3rd proper argument is always the format string.  */
  const char *fmt = va_arg (ap, const char*);

  while (*fmt)
    {
      switch (*fmt++)
        {
        default:
          fputc (*(fmt-1), file);
          break;

        case '\\':
          bs[1] = *fmt++;
          fputs (bs, file);
          break;

        case '%':
          switch (*fmt++)
            {
            case '%':
              fputc ('%', file);
              break;

            case 't':
              {
                tree t = va_arg (ap, tree);
                if (NULL_TREE == t)
                  fprintf (file, "<NULL-TREE>");
                else
                  {
                    if (stderr == file)
                      debug_tree (t);
                    else
                      {
                        print_node (file, "", t, 0);
                        putc ('\n', file);
                      }
                  }
                break;
              }

            case 'T':
              {
                tree t = va_arg (ap, tree);
                if (NULL_TREE == t)
                  fprintf (file, "<NULL-TREE>");
                else
                  print_node_brief (file, "", t, 3);
              }
              break;

            case 'd':
              fprintf (file, "%d", va_arg (ap, int));
              break;

            case 'x':
              fprintf (file, "%x", va_arg (ap, int));
              break;

            case 'b':
              fprintf (file, "%s", va_arg (ap, int) ? "true" : "false");
              break;

            case 'c':
              fputc (va_arg (ap, int), file);
              break;

            case 'r':
              print_inline_rtx (file, va_arg (ap, rtx), 0);
              break;

            case 'L':
              {
                rtx_insn *insn = safe_as_a <rtx_insn *> (va_arg (ap, rtx));

                while (insn)
                  {
                    print_inline_rtx (file, insn, 0);
                    fprintf (file, "\n");
                    insn = NEXT_INSN (insn);
                  }
                break;
              }

            case 'f':
              if (cfun && cfun->decl)
                fputs (current_function_name(), file);
              break;

            case 's':
              {
                const char *str = va_arg (ap, char*);
                fputs (str ? str : "(null)", file);
              }
              break;

            case 'm':
              fputs (GET_MODE_NAME ((machine_mode) va_arg (ap, int)),
                     file);
              break;

            case 'C':
              fputs (rtx_name[va_arg (ap, int)], file);
              break;

            case 'R':
              fputs (reg_class_names[va_arg (ap, int)], file);
              break;

            case 'F':
              fputs (caller, file);
              break;

            case 'H':
              {
                location_t loc = va_arg (ap, location_t);

                if (BUILTINS_LOCATION == loc)
                  fprintf (file, "<BUILTIN-LOCATION>");
                else if (UNKNOWN_LOCATION == loc)
                  fprintf (file, "<UNKNOWN-LOCATION>");
                else
                  fprintf (file, "%s:%d",
                           LOCATION_FILE (loc), LOCATION_LINE (loc));

                break;
              }

            case '!':
              if (!current_pass)
                return;
              /* FALLTHRU */

            case '?':
              avr_vdump (file, caller, "%F[%f:%P]");
              break;

            case 'P':
              if (current_pass)
                fprintf (file, "%s(%d)",
                         current_pass->name,
                         current_pass->static_pass_number);
              else
                fprintf (file, "pass=?");

              break;

            case 'A':
              fflush (file);
              abort();

            default:
              /* Unknown %-code: Stop printing */

              fprintf (file, "??? %%%c ???%s\n", *(fmt-1), fmt);
              fmt = "";

              break;
            }
          break; /* % */
        }
    }

  fflush (file);
}


/* Called from avr.c:avr_option_override().
   Parse argument of -mlog= and set respective fields in avr_log.  */

void
avr_log_set_avr_log (void)
{
  bool all = TARGET_ALL_DEBUG != 0;

  if (all)
    avr_log_details = "all";
	
  if (all || avr_log_details)
    {
      /* Adding , at beginning and end of string makes searching easier.  */

      char *str = (char*) alloca (3 + strlen (avr_log_details));
      bool info;

      str[0] = ',';
      strcat (stpcpy (str+1, avr_log_details), ",");

      all |= NULL != strstr (str, ",all,");
      info = NULL != strstr (str, ",?,");

      if (info)
        fprintf (stderr, "\n-mlog=");

#define SET_DUMP_DETAIL(S)                                       \
      do {                                                       \
        avr_log.S = (all || NULL != strstr (str, "," #S ","));   \
        if (info)                                                \
          fprintf (stderr, #S ",");                              \
      } while (0)

      SET_DUMP_DETAIL (address_cost);
      SET_DUMP_DETAIL (builtin);
      SET_DUMP_DETAIL (constraints);
      SET_DUMP_DETAIL (insn_addresses);
      SET_DUMP_DETAIL (legitimate_address_p);
      SET_DUMP_DETAIL (legitimize_address);
      SET_DUMP_DETAIL (legitimize_reload_address);
      SET_DUMP_DETAIL (progmem);
      SET_DUMP_DETAIL (rtx_costs);

#undef SET_DUMP_DETAIL

      if (info)
        fprintf (stderr, "?\n\n");
    }
}
