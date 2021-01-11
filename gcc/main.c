/* main.c: defines main() for cc1, cc1plus, etc.
   Copyright (C) 2007-2021 Free Software Foundation, Inc.

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
#include "tm.h"
#include "diagnostic-core.h"
#include "toplev.h"

int main (int argc, char **argv);

/* We define main() to call toplev::main(), which is defined in toplev.c.
   We do this in a separate file in order to allow the language front-end
   to define a different main(), if it so desires.  */

int
main (int argc, char **argv)
{
  toplev toplev (NULL, /* external_timer */
		 true /* init_signals */);

  int r = toplev.main (argc, argv);
#if CHECKING_P
  toplev.finalize ();
#endif

  return r;
}
