/* Message translation utilities.
   Copyright (C) 2001 Free Software Foundation, Inc.

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
#include "intl.h"

#ifdef ENABLE_NLS

/* Initialize the translation library for GCC.  This performs the
   appropriate sequence of calls - setlocale, bindtextdomain,
   textdomain.  LC_CTYPE determines the character set used by the
   terminal, so it has be set to output messages correctly.  */

void
gcc_init_libintl ()
{
#ifdef HAVE_LC_MESSAGES
  setlocale (LC_CTYPE, "");
  setlocale (LC_MESSAGES, "");
#else
  setlocale (LC_ALL, "");
#endif

  (void) bindtextdomain ("gcc", LOCALEDIR);
  (void) textdomain ("gcc");
}

#endif
