/* Operating system specific defines for BeOS target.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com), based on cygwin32.h.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */


/* Get all the PE support related things.  */
#include "cygwin32.h"

/* Change debugging to Dwarf2.  */
#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Support the __declspec keyword by turning them into attributes.
   We currently only support: dllimport and dllexport.
   Note that the current way we do this may result in a collision with
   predefined attributes later on.  This can be solved by using one attribute,
   say __declspec__, and passing args to it.  The problem with that approach
   is that args are not accumulated: each new appearance would clobber any
   existing args.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__BEOS__ -D__INTEL__ -D_X86_=1 \
-D__stdcall=__attribute__((__stdcall__)) \
-D__cdecl=__attribute__((__cdecl__)) \
-D__declspec(x)=__attribute__((x)) \
-Asystem(beos)"

#undef CPP_SPEC
#define CPP_SPEC "-remap %(cpp_cpu) %{posix:-D_POSIX_SOURCE}"

#undef LIB_SPEC
#define LIB_SPEC "-lroot -lbe -ltracker -lmedia -lnet -lnetdev -ldevice -lmidi -lgame -latalk -lmail"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "/boot/develop/lib/x86/start_dyn.o /boot/develop/lib/x86/init_term_dyn.o /boot/develop/lib/x86/glue-noinit.a"

/* Temporary. */
#define LINKERSCRIPT_SPEC "%{!T:-Tbeos.ld}"

/* No math library. */
#define MATH_LIBRARY ""

/* Don't ignore dllimport for functions.  */
#undef TARGET_NOP_FUN_DLLIMPORT
#define TARGET_NOP_FUN_DLLIMPORT 0

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES

/* Disable DWARF2 unwind info; this doesn't appear to work on
   COFF-based targets right now. (I want to say "duh?" but someone
   will correct me later. */

#undef INCOMING_RETURN_ADDR_RTX
#undef DWARF2_UNWIND_INFO

/* In the current BeOS release (DR9), use of gcc's builtin alloca is a
   problem because of the relatively low default stack size of 256K with no
   way to expand it.  So anything we compile for the BeOS target should not
   use the builtin alloca.  Defining SMALL_STACK disables builtin alloca.  */

#define SMALL_STACK

/* Yuck. */
#ifndef CROSS_COMPILE
#undef INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS \
    { \
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1 },\
    { GCC_INCLUDE_DIR, "GCC", 0, 0 },\
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1}, \
    { "/boot/develop/headers/be/add-ons/graphics", 0, 0, 0 },\
    { "/boot/develop/headers/be/translation", 0, 0, 0 },\
    { "/boot/develop/headers/be/mail", 0, 0, 0 },\
    { "/boot/develop/headers/gnu", 0, 0, 0 },\
    { "/boot/develop/headers/be/drivers", 0, 0, 0 },\
    { "/boot/develop/headers/be/game", 0, 0, 0 },\
    { "/boot/develop/headers/be/support", 0, 0, 0 },\
    { "/boot/develop/headers/be/storage", 0, 0, 0 },\
    { "/boot/develop/headers/be/kernel", 0, 0, 0 },\
    { "/boot/develop/headers/be/net", 0, 0, 0 },\
    { "/boot/develop/headers/be/midi", 0, 0, 0 },\
    { "/boot/develop/headers/be/media", 0, 0, 0 },\
    { "/boot/develop/headers/be/interface", 0, 0, 0 },\
    { "/boot/develop/headers/be/device", 0, 0, 0 },\
    { "/boot/develop/headers/be/app", 0, 0, 0 },\
    { "/boot/develop/headers/cpp", 0, 0, 0 },\
    { "/boot/develop/headers/posix", 0, 0, 0 },\
    { "/boot/develop/headers/be/precompiled", 0, 0, 0 },\
    { "/boot/develop/headers/be", 0, 0, 0 },\
    { "/boot/develop/headers", 0, 0, 0 }, \
    { 0, 0, 0, 0 } \
    };
#endif

/* Whee.  LIBRARY_PATH is Be's LD_LIBRARY_PATH, which of course will
   cause nasty problems if we override it.  */
#define LIBRARY_PATH_ENV	"BELIBRARIES"
