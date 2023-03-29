/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             T A R G E X T                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *        Copyright (C) 2005-2023, Free Software Foundation, Inc.           *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file contains target-specific parameters describing the file
    extension for object and executable files.  It is used by the compiler,
    binder, library and tools.
    Note that, in order to have access to the TARGET_* macros used below,
    the file must be compiled with IN_GCC defined, even for the library.  */

#ifdef IN_RTS

#ifndef STANDALONE
#include "tconfig.h"
#include "tsystem.h"
#endif

#else
#include "config.h"
#include "system.h"
#endif

#ifndef STANDALONE
#include "coretypes.h"
#include "tm.h"
#endif

#ifndef TARGET_OBJECT_SUFFIX
#define TARGET_OBJECT_SUFFIX ".o"
#endif

#ifndef TARGET_EXECUTABLE_SUFFIX
#define TARGET_EXECUTABLE_SUFFIX ""
#endif

#ifdef __cplusplus
extern "C" {
#endif

const char *__gnat_target_object_extension = TARGET_OBJECT_SUFFIX;
const char *__gnat_target_executable_extension = TARGET_EXECUTABLE_SUFFIX;
const char *__gnat_target_debuggable_extension = TARGET_EXECUTABLE_SUFFIX;

#ifdef __cplusplus
}
#endif
