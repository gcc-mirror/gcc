/* RTL specific diagnostic subroutines for the GNU C compiler
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include "rtl.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "input.h"
#include "toplev.h"
#include "intl.h"
#include "diagnostic.h"

static void file_and_line_for_asm PARAMS ((rtx, const char **, int *));
static void diagnostic_for_asm PARAMS ((rtx, const char *, va_list *,
                                        diagnostic_t));

/* Figure file and line of the given INSN.  */
static void
file_and_line_for_asm (insn, pfile, pline)
     rtx insn;
     const char **pfile;
     int *pline;
{
  rtx body = PATTERN (insn);
  rtx asmop;

  /* Find the (or one of the) ASM_OPERANDS in the insn.  */
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    asmop = SET_SRC (body);
  else if (GET_CODE (body) == ASM_OPERANDS)
    asmop = body;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    asmop = SET_SRC (XVECEXP (body, 0, 0));
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    asmop = XVECEXP (body, 0, 0);
  else
    asmop = NULL;

  if (asmop)
    {
      *pfile = ASM_OPERANDS_SOURCE_FILE (asmop);
      *pline = ASM_OPERANDS_SOURCE_LINE (asmop);
    }
  else
    {
      *pfile = input_filename;
      *pline = lineno;
    }
}

/* Report a diagnostic MESSAGE (an errror or a WARNING) at the line number
   of the insn INSN.  This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */
static void
diagnostic_for_asm (insn, msg, args_ptr, kind)
     rtx insn;
     const char *msg;
     va_list *args_ptr;
     diagnostic_t kind;
{
  diagnostic_info diagnostic;

  diagnostic_set_info (&diagnostic, msg, args_ptr, NULL, 0, kind);
  file_and_line_for_asm (insn, &diagnostic.location.file,
                         &diagnostic.location.line);
  report_diagnostic (&diagnostic);
}

void
error_for_asm VPARAMS ((rtx insn, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, rtx, insn);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_for_asm (insn, msgid, &ap, DK_ERROR);
  VA_CLOSE (ap);
}

void
warning_for_asm VPARAMS ((rtx insn, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, rtx, insn);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_for_asm (insn, msgid, &ap, DK_WARNING);
  VA_CLOSE (ap);
}

void
_fatal_insn (msgid, insn, file, line, function)
     const char *msgid;
     rtx insn;
     const char *file;
     int line;
     const char *function;
{
  error ("%s", _(msgid));

  /* The above incremented error_count, but isn't an error that we want to
     count, so reset it here.  */
  errorcount--;

  debug_rtx (insn);
  fancy_abort (file, line, function);
}

void
_fatal_insn_not_found (insn, file, line, function)
     rtx insn;
     const char *file;
     int line;
     const char *function;
{
  if (INSN_CODE (insn) < 0)
    _fatal_insn ("unrecognizable insn:", insn, file, line, function);
  else
    _fatal_insn ("insn does not satisfy its constraints:",
		insn, file, line, function);
}

