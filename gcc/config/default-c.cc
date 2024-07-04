/* Default C-family target hooks initializer.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.

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
#include "c-family/c-target.h"
#include "c-family/c-target-def.h"

/* Do not include tm.h or tm_p.h here; if it is useful for a target to
   define some macros for the initializer in a header without defining
   targetcm itself (for example, because of interactions with some
   hooks depending on the target OS and others on the target
   architecture), create a separate tm_c.h for only the relevant
   definitions.  */

struct gcc_targetcm targetcm = TARGETCM_INITIALIZER;
