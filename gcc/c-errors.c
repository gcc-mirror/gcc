/* Various diagnostic subroutines for the GNU C language.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-tree.h"
#include "tm_p.h"
#include "flags.h"
#include "diagnostic.h"

/* Issue an ISO C99 pedantic warning MSGID.  */

void
pedwarn_c99 VPARAMS ((const char *msgid, ...))
{
  diagnostic_info diagnostic;
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  diagnostic_set_info (&diagnostic, msgid, &ap, input_filename, lineno,
                       flag_isoc99 ? pedantic_error_kind () : DK_WARNING);
  report_diagnostic (&diagnostic);
  VA_CLOSE (ap);
}
