/* Wrapper around <objc/runtime.h>
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Nicola Pero

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

#ifndef _TESTSUITE_RUNTIME_H_
#define _TESTSUITE_RUNTIME_H_

/* Include this file where you'd normally include <objc/runtime.h>.

   Older versions of the NeXT runtime do not have <objc/runtime.h> and
   you need to include <objc/objc-runtime.h> instead.  This file takes
   care of figuring out if that's the case.  */

#ifndef __NEXT_RUNTIME__

/*
  GNU Objective-C runtime (libobjc).
*/
# include <objc/runtime.h>

#else

/*
  NeXT Objective-C runtime.
*/

/* Include next-abi.h to determine which version of the runtime we are
   dealing with.  TODO: If this is the only place including it, maybe
   it could be copied here ?  */
# include "next-abi.h"

# ifdef NEXT_OBJC_USE_NEW_INTERFACE

/* New NeXT runtime, with an API that should be basically identical to
   the GNU Objective-C one.  */
#  include <objc/runtime.h>

# else

/* Old NeXT runtime, with an API similar, but not identical to the new
   one.  To start with, different headers need to be included.  */
#  include <objc/objc-class.h>
#  include <objc/objc-runtime.h>

/* Not all functions are available in the old NeXT runtime.  A few
   that we need are not, and here we provide an implementation on top
   of the old NeXT API.  */

#  define class_isMetaClass(C) (CLS_GETINFO((struct objc_class *)C, CLS_META)? YES: NO)
#  define class_getName(C) object_getClassName(C)
#  define class_getSuperclass(C)  (((struct objc_class *)C)->super_class)
#  define method_getImplementation(M) (((Method)M)->method_imp)
#  define method_getTypeEncoding(M) (((Method)M)->method_types)
#  define object_getClass(O) (*(struct objc_class **)O)

#include <objc/Protocol.h>
BOOL class_conformsToProtocol (Class class_, Protocol *protocol)
{
  struct objc_protocol_list *p;
  int i;
  for (p = class_->protocols; p; p = p->next)
    for (i = 0; i < p->count; i++)
      if ([p->list[i] conformsTo: protocol])
	return YES;
  return NO;
}

#define protocol_getName(P) [P name]
#define protocol_isEqual(P,Q) [P isEqual: Q]

struct objc_method_description protocol_getMethodDescription (Protocol *protocol, 
							      SEL selector,
							      BOOL requiredMethod,
							      BOOL instanceMethod)
{
  struct objc_method_description *tmp;
  struct objc_method_description result;

  if (instanceMethod)
    tmp = [protocol descriptionForInstanceMethod: selector];
  else
    tmp = [protocol descriptionForClassMethod: selector];

  if (tmp)
    result = *tmp;
  else
    {
      result.name = (SEL)0;
      result.types = (char *)0;
    }

  return result;
}

#  endif /* NEXT_OBJC_USE_NEW_INTERFACE */

# endif /* __NEXT_RUNTIME__ */

#endif /* _TESTSUITE_RUNTIME_H_ */

