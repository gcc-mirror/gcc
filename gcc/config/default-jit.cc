/* Default JIT language target hooks initializer.
   Copyright (C) 2023 Free Software Foundation, Inc.

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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm_jit.h"
#include "jit/jit-target.h"
#include "jit/jit-target-def.h"

/* Do not include tm.h or tm_p.h here; definitions needed by the target
   architecture to initialize targetjitm should instead be added to tm_jit.h.
   */

struct gcc_targetjitm targetjitm = TARGETJITM_INITIALIZER;
