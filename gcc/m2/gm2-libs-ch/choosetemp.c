/* choosetemp.c provide access to temporary file creation.

Copyright (C) 2005-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "libiberty.h"
#include "Gchoosetemp.h"

#   ifdef __cplusplus
extern "C" {
#   endif

/* Return a temporary file name (as a string) or NIL if unable to
create one.  */

void *
choosetemp_make_temp_file (void *suffix)
{
  return (void *)make_temp_file ((const char *)suffix);
}

/* to satisfy the GM2 linker.  */
void
_M2_choosetemp_init (void)
{
}

/* to satisfy the GM2 linker.  */
void
_M2_choosetemp_finish (void)
{
}
#   ifdef __cplusplus
}
#   endif
