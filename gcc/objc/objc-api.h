/* GNU Objective-C Runtime API.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files compiled
   with GCC to produce an executable, this does not cause the resulting
   executable to be covered by the GNU General Public License.  This
   exception does not however invalidate any other reasons why the
   executable file might be covered by the GNU General Public License. */

#ifndef __objc_api_INCLUDE_GNU
#define __objc_api_INCLUDE_GNU

#include <stdlib.h>
#include <objc/objc.h>

static const ARGSIZE = 96;		/* for `method_get_argsize()' */

/*
** Points to the function that the runtime system calls to handle
** an error.  By default, it prints formatted error messages to the
** standard error stream and calls abort to produce a core file.
** The function is guaranteed to be passed a valid object and a
** non-NULL format string.
*/
extern void (*_objc_error)(id object, const char *format, va_list args);

/*
** This is a hook which is called by objc_lookup_class and
** objc_get_class if the runtime is not able to find the class.
** This may e.g. try to load in the class using dynamic loading.
** The function is guaranteed to be passed a non-NULL name string.
*/
extern Class_t (*_objc_lookup_class)(const char *name);

/*
** Points to the function that the runtime system calls to allocate
** memory for new instances.  Called through class_create_instance(),
** this function should return a valid block of memory of at least
** class_get_instance_size() bytes, or nil upon failure.  The
** function is guaranteed to be passed a valid class object.
*/
extern id (*_objc_object_alloc)(Class_t class);

/*
** Points to the function that the runtime system calls to create
** an exact copy of an object.  Called through object_copy(), this
** function should return a new instance of object's class created
** by class_create_instance() which is bit-identical to object, or
** nil upon failure.  The function is guaranteed to be passed a
** valid instance object.
*/
extern id (*_objc_object_copy)(id object);

/*
** Points to the function that the runtime system calls to free
** instances.  Called through object_dispose(), this function
** should free the memory pointed to by object and return nil.
** This function is not responsible for freeing memory pointed
** to by any of the object's instance variables.  The function
** is guaranteed to be passed a valid instance object.
*/
extern id (*_objc_object_dispose)(id object);

/*
** Searches for a class method specified by aSel, starting with the
** metaclass class and proceeding up the class hierarchy, until either
** the method is found or the root class has been examined.  Returns
** a pointer to the method's Method structure if found.  Returns the
** value METHOD_NULL if the method is not found, class is not a
** metaclass object, or aSel is not a valid selector.
*/
Method_t class_get_class_method(MetaClass_t class, SEL aSel);

/*
** Searches for an instance method specified by aSel, starting with
** the class class and proceeding up the class hierarchy, until either
** the method is found or the root class has been examined.  Returns
** a pointer to the method's Method structure if found.  Returns the
** value METHOD_NULL if the method is not found, class is not a class
** object, or aSel is not a valid selector.
*/
Method_t class_get_instance_method(Class_t class, SEL aSel);

/*
** Causes impostor to pose as its superclass.  Messages sent to the
** superclass will actually be sent to the posing class.  Instance
** variables may not be declared in the posing class.  The posing
** class can add new methods to the class or override existing methods
** in the superclass.  Returns non-nil on success.  Returns nil if
** either of impostor or superclass are not class objects, impostor is
** not a subclass of superclass, or upon some other error.
*/
Class_t class_pose_as(Class_t impostor, Class_t superclass);

/*
** Returns the class object for the class named name.  If name does not
** identify a known class, the hook _objc_lookup_class is called.  If
** this fails, an error message is issued and the system aborts.
*/
Class_t objc_get_class(const char *name);

/*
** Returns the class object for the class named name.  If name does not
** identify a known class, the hook _objc_lookup_class is called.  If
** this fails, nil is returned.
*/
Class_t objc_lookup_class(const char *name);

/*
** Returns the method name associated with selector, or NULL
** if selector is not defined.
*/
const char *sel_get_name(SEL selector);

/*
** Returns the selector associated with the method name name.  If name
** has not been defined or name is NULL, 0 is returned.
*/
SEL sel_get_uid(const char *name);

/*
** Registers a selector for name and returns the new selector.  If
** name is NULL or the empty string (""), 0 is returned.
*/
SEL sel_register_name(const char *name);

/*
** Indicate if aSel is a valid selector.  This is not a safe
** operation, and it should really never be nessecary to use.
*/
BOOL sel_is_mapped (SEL aSel);

/*******************************************************************/
/*                                                                 */
/* Internal __inline functions                                     */
/*                                                                 */
/*******************************************************************/

/*
** Allocates memory for a new object of class class by calling the
** function specified by the variable _objc_object_alloc if non-zero,
** otherwise uses a default method.  Then, initializes the object's
** isa instance variable to class, and returns the new object.
** Returns nil if the memory could not be allocated or class is not
** a class object.
*/
extern inline id
class_create_instance(Class_t class)
{
  id new = nil;
  if (CLS_ISCLASS(class))
    new = (_objc_object_alloc
           ?(*_objc_object_alloc)(class)
           :(id)malloc(class->instance_size));
  if (new!=nil)
    new->class_pointer = class;
  return new;
}

/*
** Returns name of the class class or empty string if class is not
** a class object.  If class is Nil, the string "Nil" is returned.
*/
static inline const char *
class_get_class_name(Class_t class)
{
  return CLS_ISCLASS(class)?class->name:((class==Nil)?"Nil":0);
}

/*
** Returns the size of an instance of class class in bytes, or 0 if
** class is not a class.  The size of an instance is at least 4 bytes.
*/
static inline long
class_get_instance_size(Class_t class)
{
  return CLS_ISCLASS(class)?class->instance_size:0;
}

/*
** Returns a pointer to class's metaclass, or Nil if class is not a
** class.
*/
static inline MetaClass_t
class_get_meta_class(Class_t class)
{
  return CLS_ISCLASS(class)?class->class_pointer:Nil;
}

/*
** Returns a pointer to class's superclass, or Nil if class is not a
** class.  Note that the superclass of Object is Nil.
*/
static inline Class_t
class_get_super_class(Class_t class)
{
  return CLS_ISCLASS(class)?class->super_class:Nil;
}

/*
** Returns the version number for the class, or -1 if class is not a
** class.
*/
static inline int
class_get_version(Class_t class)
{
  return CLS_ISCLASS(class)?class->version:-1;
}

/*
** Returns YES if class is a class, or NO if not.
*/
static inline BOOL
class_is_class(Class_t class)
{
  return CLS_ISCLASS(class);
}

/*
** Returns YES if class is a metaclass, or NO if not.
*/
static inline BOOL
class_is_meta_class(Class_t class)
{
  return CLS_ISMETA(class);
}


/*
** Sets the version number of class class.  Does nothing if class is
** not a class.
*/
static inline void
class_set_version(Class_t class, long version)
{
  if (CLS_ISCLASS(class))
    class->version = version;
}

/*
** Returns the size in bytes of the argument frame to a method.  Since
** at least two parameters (self and _cmd) are sent to each method, this
** value will be at least 8.  If method is not a valid method, 0 is
** returned.
** 
** Currently, the frame size info is only reliable on a NeXT, so until
** we get this fixed, we'll use a value which is most possibly large
** enough. You can possibly reduce this value (96) on anything but a
** Sparc if you don't return structs from the methods forwarded to.
*/
static inline unsigned int
method_get_argsize(Method_t method)
{
  return ARGSIZE;		/* This was a magic number (96)... */
}

/*
** Returns a pointer to the implementation of method method.  If method
** is not a method, NULL is returned.
*/
static inline IMP
method_get_imp(Method_t method)
{
  return (method!=METHOD_NULL)?method->method_imp:(IMP)0;
}

/*
** Returns the implementation (pointer to function) of the method
** identified by a (class, selector) pair.  Use this, and *not* 
** objc_msg_lookup, since objc_msg_lookup may eventually return a
** pointer to an internal function which does lazy initialization...
*/
IMP get_imp (Class_t class, SEL sel);

/*
** Creates a new instance object that's an exact copy of object by
** calling the function pointed to by the variable _objc_object_copy if
** non-zero, otherwise uses a default method.  Returns the new object.
** Returns nil if object is not an instance object, memory for the new
** object could not be allocated, or some other error occurred.
*/
extern inline id
object_copy(id object)
{
  if ((object!=nil)&&CLS_ISCLASS(object->class_pointer))
    {
      if (_objc_object_copy)
        return (*_objc_object_copy)(object);
      else
        {
          id copy = class_create_instance(object->class_pointer);
          if (copy!=nil)
            bcopy(object, copy, object->class_pointer->instance_size);
          return copy;
        }
      return nil;
    }
}

/*
** Frees the memory occupied by object by calling the function pointed
** to by the variable _objc_object_dispose if non-zero, otherwise uses
** a default method.  Always returns nil.  If object is not an instance
** object, does nothing.
*/
extern inline id
object_dispose(id object)
{
  if ((object!=nil)&&CLS_ISCLASS(object->class_pointer))
    {
      if (_objc_object_dispose)
        (*_objc_object_dispose)(object);
      else
        free(object);
    }
  return nil;
}

/*
** Returns the class of an object.  If object is an instance, this is
** its class object.  If object is a class object, returns object (this
** is arguably not correct, but is implemented this way for compatibility
** with NeXT (and Stepstone?)).  If object is a metaclass object, or
** object is nil, returns Nil.
*/
static inline Class_t
object_get_class(id object)
{
  return ((object!=nil)
	  ? (CLS_ISCLASS(object->class_pointer)
	     ? object->class_pointer
	     : (CLS_ISMETA(object->class_pointer)
		? (Class_t)object
		: Nil))
	  : Nil);
}

/*
** Returns the name of the class of object.  If object is an instace,
** this is the name of its class.  If object is a class or a metaclass,
** returns its name.  If object is nil, returns "Nil".
*/
static inline const char *
object_get_class_name(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->name
                         :((Class_t)object)->name)
                       :"Nil");
}

/*
** Returns the metaclass of an object.  If object is an instance or a
** class, this is the metaclass object for it.  If object is a metaclass
** object, or object is nil, returns Nil.
*/
static inline MetaClass_t
object_get_meta_class(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->class_pointer
                         :(CLS_ISMETA(object->class_pointer)
                           ?object->class_pointer
                           :Nil))
                       :Nil);
}

/*
** Returns the superclass of object.  If object is an instance or
** a class, this is its superclass-object for it.  If object is a
** metaclass-object or nil, this is Nil.
*/
static inline Class_t
object_get_super_class(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->super_class
                         :(CLS_ISMETA(object->class_pointer)
                           ?((Class_t)object)->super_class
                           :Nil))
                       :Nil);
}

/*
** YES if object is a class, NO if not.
*/
static inline BOOL
object_is_class(id object)
{
  return CLS_ISCLASS((Class_t)object);
}

/*
** YES if object is an instance, NO if not.
*/
static inline BOOL
object_is_instance(id object)
{
  return (object!=nil)&&CLS_ISCLASS(object->class_pointer);
}

/*
** YES if object is a metaclass, NO if not.
*/
static inline BOOL
object_is_meta_class(id object)
{
  return CLS_ISMETA((Class_t)object);
}

/*
** Functions used for archiving.  This is not documented yet!
*/

TypedStream* new_typed_stream(FILE* physical);
void free_typed_stream(TypedStream* stream);

void objc_write_object(TypedStream* stream, id object);
int objc_read_object(TypedStream* stream, id *object);

void objc_write_type(TypedStream* stream, const char* type, const void* data);
void objc_read_type(TypedStream* stream, const char* type, void* data);

#endif /* not __objc_api_INCLUDE_GNU */
