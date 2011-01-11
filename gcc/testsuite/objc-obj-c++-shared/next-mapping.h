/* Compatibility header between runtimes and APIs.
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.

   Original Authors: Ziemowit Laski <zlaski@apple.com>
		     David Ayers <d.ayers@inode.at>
		     
   re-work for ObjC2 by Iain Sandoe <iains@gcc.gnu.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _OBJC_NEXT_MAPPING_H_
#define _OBJC_NEXT_MAPPING_H_

/* This file provides a two-way mapping of API names for the original 
   GNU & NeXT APIs. 
   
   It is being expanded to provide mapping (where possible) between between the
   older API and API-2.
*/

#include "objc-test-suite-types.h"

#ifndef __NEXT_RUNTIME__

# define CLASSFIELD class_pointer
# define CLASSPTRFIELD(x) (x)->class_pointer
# define SUPERCLASS superClass
# define OBJC_GETCLASS objc_get_class

# ifdef __objc_api_INCLUDE_GNU
#  define class_createInstance(C, S) class_create_instance(C)
# endif
# define method_get_types(M) (M)->method_types

#else /* NeXT */

/* Include next-abi.h to set NEXT_OBJC_USE_NEW_INTERFACE etc.*/
# include "next-abi.h"

# ifdef NEXT_OBJC_USE_NEW_INTERFACE
   /* API=2. */
#  include <objc/runtime.h>
# else
   /* API=0. */
#  include <objc/objc-class.h>
# endif

# define CLASSPTRFIELD(x) (x)->isa
# define SUPERCLASS superclass
# define OBJC_GETCLASS objc_getClass

# define objc_get_class(C) objc_getClass(C)
# define objc_get_meta_class(C) objc_getMetaClass(C)
# define class_get_class_method(C, S) class_getClassMethod(C, S)
# define class_get_instance_method(C, S) class_getInstanceMethod(C, S)
# define sel_get_name(S) sel_getName(S)
# define class_create_instance(C) class_createInstance(C, 0)
# define class_get_class_name(C) object_getClassName(C)
# define objc_lookup_class(N) objc_lookUpClass(N)

# ifdef NEXT_OBJC_USE_NEW_INTERFACE

#  define object_class_name(O) (object_getClassName(O)) 
#  define object_get_class(O) (object_getClass((id)O))
#  define object_get_super_class(O) class_get_super_class(object_get_class(O))
#  define object_is_class(O) class_is_meta_class(object_get_class(O))
#  define object_is_meta_class(O) (object_is_class(O) && class_is_meta_class(O) \
						 && class_is_meta_class(object_get_class(O)))

#  define method_get_imp(M) (method_getImplementation((Method)M))
#  define method_get_types(M) (method_getTypeEncoding((Method)M))

#  define class_get_super_class(C) (class_getSuperclass((Class)C))
#  define class_is_meta_class(C) (class_isMetaClass((Class)C) ? YES: NO)
#  define class_is_class(C) (class_is_meta_class(C) == NO)

# else /* OLD API */

#  define object_class_name(O) (O->name) 
#  define object_get_super_class(O) class_get_super_class(*(struct objc_class **)O)
#  define object_get_class(O) (*(struct objc_class **)O)
#  define object_is_class(O) class_is_meta_class(*(struct objc_class **)O)
#  define object_is_meta_class(O) (class_is_meta_class(O) && class_is_meta_class(*(struct objc_class **)O))

#  define method_get_imp(M) (((Method)M)->method_imp)
#  define method_get_types(M) (((Method)M)->method_types)

#  define class_get_super_class(C) (((struct objc_class *)C)->super_class)
#  define class_is_meta_class(C) (CLS_GETINFO((struct objc_class *)C, CLS_META)? YES: NO)
#  define class_is_class(C) (CLS_GETINFO((struct objc_class *)C, CLS_CLASS)? YES: NO)

# endif /* NEXT_OBJC_USE_NEW_INTERFACE */

# endif  /*__NEXT_RUNTIME__ */
#endif /* _OBJC_NEXT_MAPPING_H_ */