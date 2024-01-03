/* Linux support needed only by Rust front-end.
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

/* `-mandroid' is not available as an command-line option.  */
#ifndef TARGET_ANDROID
#define TARGET_ANDROID 0
#endif

/* Implement TARGET_RUST_OS_INFO for Linux targets.  */

static void
linux_rust_target_os_info (void)
{
  rust_add_target_info ("target_family", "unix");
  rust_add_target_info ("target_vendor", "unknown");

  if (TARGET_ANDROID)
    rust_add_target_info ("target_os", "android");
  else
    rust_add_target_info ("target_os", "linux");

  if (OPTION_GLIBC)
    rust_add_target_info ("target_env", "gnu");
  else if (OPTION_MUSL)
    rust_add_target_info ("target_env", "musl");
  else if (OPTION_UCLIBC)
    rust_add_target_info ("target_env", "uclibc");
  else
    rust_add_target_info ("target_env", "");
}

#undef TARGET_RUST_OS_INFO
#define TARGET_RUST_OS_INFO linux_rust_target_os_info

struct gcc_targetrustm targetrustm = TARGETRUSTM_INITIALIZER;
