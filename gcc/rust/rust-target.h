/* rust-target.h -- Data structure definitions for target-specific Rust behavior.
   Copyright (C) 2020 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_RUST_TARGET_H
#define GCC_RUST_TARGET_H

// TODO: find out what this stuff actually does
#define DEFHOOKPOD(NAME, DOC, TYPE, INIT) TYPE NAME;
#define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (* NAME) PARAMS;
#define DEFHOOK_UNDOC DEFHOOK
#define HOOKSTRUCT(FRAGMENT) FRAGMENT

#include "rust-target.def"

/* Each target can provide their own.  */
extern struct gcc_targetrustm targetrustm;
/* Some kind of structure to store all rust hook macros (like the TARGET_RUST_CPU_INFO).
 * This is required to store the function pointers for the target hooks so that the frontend can call them
 * and it calls the correct target-specific function.  */

/* Used by target to add predefined version idenditiers.  */
//extern void d_add_builtin_version (const char *);
/* Used by target to add target-related info.  */
extern void rust_add_target_info(const char* key, const char* value);

#endif