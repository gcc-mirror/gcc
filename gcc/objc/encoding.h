/* Encoding of types for Objective C.
   Copyright (C) 1993, 1997 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

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
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef __encoding_INCLUDE_GNU
#define __encoding_INCLUDE_GNU

#include <ctype.h>
#include "objc/objc-api.h"

#define _C_CONST    'r'
#define _C_IN       'n'
#define _C_INOUT    'N'
#define _C_OUT      'o'
#define _C_BYCOPY   'O'
#define _C_ONEWAY   'V'

#define _F_CONST    0x01
#define _F_IN       0x01
#define _F_OUT      0x02
#define _F_INOUT    0x03
#define _F_BYCOPY   0x04
#define _F_ONEWAY   0x08


int objc_aligned_size (const char* type);
int objc_sizeof_type (const char* type);
int objc_alignof_type (const char* type);
int objc_aligned_size (const char* type);
int objc_promoted_size (const char* type);
const char* objc_skip_type_qualifiers (const char* type);
const char* objc_skip_typespec (const char* type);
const char* objc_skip_offset (const char* type);
const char* objc_skip_argspec (const char* type);
int method_get_number_of_arguments (struct objc_method*);
int method_get_sizeof_arguments (struct objc_method*);

char* method_get_first_argument (struct objc_method*,
				 arglist_t argframe, 
				 const char** type);
char* method_get_next_argument (arglist_t argframe, 
				const char **type);
char* method_get_nth_argument (struct objc_method* m, 
			       arglist_t argframe,
			       int arg, 
			       const char **type);

unsigned objc_get_type_qualifiers (const char* type);


#endif /* __encoding_INCLUDE_GNU */
