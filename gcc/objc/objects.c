/* GNU Objective C Runtime class related functions
   Copyright (C) 1993 Free Software Foundation, Inc.

Author: Kresten Krab Thorup

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

You should have received a copy of the GNU General Public License along with
   GNU CC; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#include "runtime.h"		/* the kitchen sink */

id __objc_object_alloc(Class_t);
id __objc_object_dispose(id);
id __objc_object_copy(id);

id (*_objc_object_alloc)(Class_t) = __objc_object_alloc;
id (*_objc_object_dispose)(id)    = __objc_object_dispose;
id (*_objc_object_copy)(id)       = __objc_object_copy;

id
class_create_instance(Class_t class)
{
  id res = (*_objc_object_alloc)(class);
  res->class_pointer = class;
  return res;
}

id 
object_copy(id object)
{
  return (*_objc_object_copy)(object);
}

id 
object_dispose(id object)
{
  return (*_objc_object_dispose)(object);
}

id __objc_object_alloc(Class_t class)
{
  return (id)__objc_xmalloc(class->instance_size);
}

id __objc_object_dispose(id object) 
{
  free(object);
  return 0;
}

id __objc_object_copy(id object)
{
  id copy = class_create_instance(object->class_pointer);
  bcopy(object, copy, object->class_pointer->instance_size);
  return copy;
}


