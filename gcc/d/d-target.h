/* d-target.h -- Data structure definitions for target-specific D behavior.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.

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

#ifndef GCC_D_TARGET_H
#define GCC_D_TARGET_H

#define DEFHOOKPOD(NAME, DOC, TYPE, INIT) TYPE NAME;
#define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) TYPE (* NAME) PARAMS;
#define DEFHOOK_UNDOC DEFHOOK
#define HOOKSTRUCT(FRAGMENT) FRAGMENT

#include "d-target.def"

/* Each target can provide their own.  */
extern struct gcc_targetdm targetdm;

/* Used by target to add predefined version idenditiers.  */
extern void d_add_builtin_version (const char *);

/* Structure describing a supported key for `__traits(getTargetInfo)' and a
   function to handle it.  */
struct d_target_info_spec
{
  /* The name of the key or NULL to mark the end of a table of keys.  */
  const char *name;
  /* Function to handle this key, the return value of the handler must be a CST.
     This pointer may be NULL if no special handling is required, for instance,
     the key must always be available according to the D language spec.  */
  tree (*handler) ();
};

/* Used by target to add getTargetInfo handlers.  */
extern void d_add_target_info_handlers (const d_target_info_spec *);

#endif /* GCC_D_TARGET_H  */
