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

#include "objc/objc.h"
#include "objc/hash.h"


static const ARGSIZE = 96;		/* for `method_get_argsize()' */

extern void (*_objc_error)(id object, const char *format, va_list args);

/*
** This is a hook which is called by objc_lookup_class and
** objc_get_class if the runtime is not able to find the class.
** This may e.g. try to load in the class using dynamic loading.
** The function is guaranteed to be passed a non-NULL name string.
*/
extern Class_t (*_objc_lookup_class)(const char *name);

extern id (*_objc_object_alloc)(Class_t class);

extern id (*_objc_object_copy)(id object);

extern id (*_objc_object_dispose)(id object);

Method_t class_get_class_method(MetaClass_t class, SEL aSel);

Method_t class_get_instance_method(Class_t class, SEL aSel);

Class_t class_pose_as(Class_t impostor, Class_t superclass);

Class_t objc_get_class(const char *name);

Class_t objc_lookup_class(const char *name);

const char *sel_get_name(SEL selector);

SEL sel_get_uid(const char *name);

SEL sel_register_name(const char *name);

BOOL sel_is_mapped (SEL aSel);

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

static inline const char *
class_get_class_name(Class_t class)
{
  return CLS_ISCLASS(class)?class->name:((class==Nil)?"Nil":0);
}

static inline long
class_get_instance_size(Class_t class)
{
  return CLS_ISCLASS(class)?class->instance_size:0;
}

static inline MetaClass_t
class_get_meta_class(Class_t class)
{
  return CLS_ISCLASS(class)?class->class_pointer:Nil;
}

static inline Class_t
class_get_super_class(Class_t class)
{
  return CLS_ISCLASS(class)?class->super_class:Nil;
}

static inline int
class_get_version(Class_t class)
{
  return CLS_ISCLASS(class)?class->version:-1;
}

static inline BOOL
class_is_class(Class_t class)
{
  return CLS_ISCLASS(class);
}

static inline BOOL
class_is_meta_class(Class_t class)
{
  return CLS_ISMETA(class);
}


static inline void
class_set_version(Class_t class, long version)
{
  if (CLS_ISCLASS(class))
    class->version = version;
}

static inline unsigned int
method_get_argsize(Method_t method)
{
  return ARGSIZE;		/* This was a magic number (96)... */
}

static inline IMP
method_get_imp(Method_t method)
{
  return (method!=METHOD_NULL)?method->method_imp:(IMP)0;
}

IMP get_imp (Class_t class, SEL sel);

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

static inline const char *
object_get_class_name(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->name
                         :((Class_t)object)->name)
                       :"Nil");
}

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

static inline Class_t
object_get_super_class
(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->super_class
                         :(CLS_ISMETA(object->class_pointer)
                           ?((Class_t)object)->super_class
                           :Nil))
                       :Nil);
}

static inline BOOL
object_is_class(id object)
{
  return CLS_ISCLASS((Class_t)object);
}

static inline BOOL
object_is_instance(id object)
{
  return (object!=nil)&&CLS_ISCLASS(object->class_pointer);
}

static inline BOOL
object_is_meta_class(id object)
{
  return CLS_ISMETA((Class_t)object);
}


/* Archiving stuff */

#ifndef __alpha__

typedef int (*objc_typed_read_func)(void*, char*, int);
typedef int (*objc_typed_write_func)(void*, const char*, int);
typedef int (*objc_typed_flush_func)(void*);
typedef int (*objc_typed_eof_func)(void*);

#define OBJC_READONLY   0x01
#define OBJC_WRITEONLY  0x02

#define OBJC_MANAGED_STREAM  0x01
#define OBJC_FILE_STREAM     0x02
#define OBJC_MEMORY_STREAM   0x04

#define OBJC_TYPED_STREAM_VERSION 0x01

struct objc_typed_stream {
  void* physical;
  cache_ptr object_table;	/* read/written objects */
  cache_ptr stream_table;	/* other read/written but shared things.. */
  cache_ptr class_table;	/* class version mapping */
  cache_ptr object_refs;	/* forward references */
  int mode;			/* OBJC_READONLY or OBJC_WRITEONLY */
  int type;			/* MANAGED, FILE, MEMORY etc bit string */
  int version;			/* version used when writing */
  int writing_root_p;
  objc_typed_read_func read;
  objc_typed_write_func write;
  objc_typed_eof_func eof;
  objc_typed_flush_func flush;
};

/* opcode masks */
#define _B_VALUE   0x1fU
#define _B_CODE    0xe0U
#define _B_SIGN    0x10U
#define _B_NUMBER  0x0fU

/* standard opcodes */
#define _B_INVALID 0x00U
#define _B_SINT    0x20U
#define _B_NINT    0x40U
#define _B_SSTR    0x60U
#define _B_NSTR    0x80U
#define _B_RCOMM   0xa0U
#define _B_UCOMM   0xc0U
#define _B_EXT     0xe0U

/* eXtension opcodes */
#define _BX_OBJECT  0x00U
#define _BX_CLASS   0x01U
#define _BX_SEL     0x02U
#define _BX_OBJREF  0x03U
#define _BX_OBJROOT 0x04U
#define _BX_EXT     0x1fU

/*
** Read and write objects as specified by TYPE.  All the `last'
** arguments are pointers to the objects to read/write.  
*/

int objc_write_type (TypedStream* stream, const char* type, const void* data);
int objc_read_type (TypedStream* stream, const char* type, void* data);

int objc_write_types (TypedStream* stream, const char* type, ...);
int objc_read_types (TypedStream* stream, const char* type, ...);

int objc_write_object_reference (TypedStream* stream, id object);
int objc_write_root_object (TypedStream* stream, id object);

int objc_get_stream_class_version (TypedStream* stream, Class* class);


/*
** Convenience funtions
*/

int objc_write_array (TypedStream* stream, const char* type,
		      int count, const void* data);
int objc_read_array (TypedStream* stream, const char* type,
		     int count, void* data);

int objc_write_object (TypedStream* stream, id object);

/*
** Open a typed stream for reading or writing.  MODE may be either of
** OBJC_READONLY or OBJC_WRITEONLY.  
*/

TypedStream* objc_open_typed_stream (FILE* physical, int mode);
TypedStream* objc_open_typed_stream_for_file (const char* file_name, int mode);

void objc_close_typed_stream (TypedStream* stream);

BOOL objc_end_of_typed_stream (TypedStream* stream);
void objc_flush_typed_stream (TypedStream* stream);

#endif /* __alpha__ */
#endif /* not __objc_api_INCLUDE_GNU */
