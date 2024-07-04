/* OpenBSD support needed only by Rust front-end.
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
#include "tm.h"
#include "tm_rust.h"
#include "rust/rust-target.h"
#include "rust/rust-target-def.h"

/* Implement TARGET_RUST_OS_INFO for OpenBSD targets.  */

static void
openbsd_rust_target_os_info (void)
{
  rust_add_target_info ("target_family", "unix");
  rust_add_target_info ("target_os", "openbsd");
  rust_add_target_info ("target_vendor", "unknown");
  rust_add_target_info ("target_env", "");
}

#undef TARGET_RUST_OS_INFO
#define TARGET_RUST_OS_INFO openbsd_rust_target_os_info

struct gcc_targetrustm targetrustm = TARGETRUSTM_INITIALIZER;
