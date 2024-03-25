/* Default Rust language target hooks initializer.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.

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
#include "tm_rust.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Do not include tm.h or tm_p.h here; definitions needed by the target
   architecture to initialize targetrustm should instead be added to tm_rust.h.
 */

struct gcc_targetrustm targetrustm = TARGETRUSTM_INITIALIZER;
