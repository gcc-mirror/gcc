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
#include <stdio.h>

/* For functions which return Method_t */
#define METHOD_NULL	(Method_t)0
                                                /* Boolean typedefs */
/*
** Method descriptor returned by introspective Object methods.
** This is really just the first part of the more complete objc_method
** structure defined below and used internally by the runtime.
*/
struct objc_method_description
{
    SEL name;			/* this is a selector, not a string */
    char *types;		/* type encoding */
};

/* Filer types used to describe Ivars and Methods.  */
#define _C_ID       '@'
#define _C_CLASS    '#'
#define _C_SEL      ':'
#define _C_CHR      'c'
#define _C_UCHR     'C'
#define _C_SHT      's'
#define _C_USHT     'S'
#define _C_INT      'i'
#define _C_UINT     'I'
#define _C_LNG      'l'
#define _C_ULNG     'L'
#define _C_FLT      'f'
#define _C_DBL      'd'
#define _C_BFLD     'b'
#define _C_VOID     'v'
#define _C_UNDEF    '?'
#define _C_PTR      '^'
#define _C_CHARPTR  '*'
#define _C_ATOM     '%'
#define _C_ARY_B    '['
#define _C_ARY_E    ']'
#define _C_UNION_B  '('
#define _C_UNION_E  ')'
#define _C_STRUCT_B '{'
#define _C_STRUCT_E '}'



/*
** Set this variable nonzero to print a line describing each
** message that is sent.  (this is currently disabled)
*/
extern BOOL objc_trace;


/*
** Whereas a Module (defined further down) is the root (typically) of a file,
** a Symtab is the root of the class and category definitions within the
** module.  
** 
** A Symtab contains a variable length array of pointers to classes and
** categories  defined in the module. 
*/
typedef struct objc_symtab {
  unsigned long sel_ref_cnt;                     /* Unknown. */
  SEL        refs;                              /* Unknown. */
  unsigned short cls_def_cnt;                   /* Number of classes compiled
                                                  (defined) in the module. */
  unsigned short cat_def_cnt;                   /* Number of categories 
                                                  compiled (defined) in the 
                                                  module. */
  void      *defs[1];                           /* Variable array of pointers.
                                                  cls_def_cnt of type Class* 
                                                  followed by cat_def_cnt of
                                                  type Category_t. */
} Symtab,   *Symtab_t;


/*
** The compiler generates one of these structures for each module that
** composes the executable (eg main.m).  
** 
** This data structure is the root of the definition tree for the module.  
** 
** A collect program runs between ld stages and creates a ObjC ctor array. 
** That array holds a pointer to each module structure of the executable. 
*/
typedef struct objc_module {
  unsigned long version;                        /* Compiler revision. */
  unsigned long size;                           /* sizeof(Module). */
  const char* name;                             /* Name of the file where the 
                                                  module was generated.   The 
                                                  name includes the path. */
  Symtab_t    symtab;                           /* Pointer to the Symtab of
                                                  the module.  The Symtab
                                                  holds an array of pointers to 
                                                  the classes and categories 
                                                  defined in the module. */
} Module, *Module_t;


/*
** The compiler generates one of these structures for a class that has
** instance variables defined in its specification. 
*/
typedef struct objc_ivar* Ivar_t;
typedef struct objc_ivar_list {
  int   ivar_count;                             /* Number of structures (Ivar) 
                                                  contained in the list.  One
                                                  structure per instance 
                                                  variable defined in the
                                                  class. */
  struct objc_ivar {
    const char* ivar_name;                      /* Name of the instance
                                                  variable as entered in the
                                                  class definition. */
    const char* ivar_type;                      /* Description of the Ivar's
                                                  type.  Useful for 
                                                  debuggers. */
    int        ivar_offset;                    /* Byte offset from the base 
                                                  address of the instance 
                                                  structure to the variable. */

  } ivar_list[1];                               /* Variable length 
                                                  structure. */
} IvarList, *IvarList_t;


/*
** The compiler generates one (or more) of these structures for a class that
** has methods defined in its specification. 
** 
** The implementation of a class can be broken into separate pieces in a file
** and categories can break them across modules. To handle this problem is a
** singly linked list of methods. 
*/
typedef struct objc_method Method;
typedef Method* Method_t;
typedef struct objc_method_list {
  struct objc_method_list*  method_next;      /* This variable is used to link 
                                                a method list to another.  It 
                                                is a singly linked list. */
  int            method_count;               /* Number of methods defined in 
                                                this structure. */
  struct objc_method {
    SEL         method_name;                  /* This variable is the method's 
                                                name.  It is a char*. 
                                                  The unique integer passed to 
                                                objc_msg_send is a char* too.  
                                                It is compared against 
                                                method_name using strcmp. */
    const char* method_types;                 /* Description of the method's
                                                parameter list.  Useful for
                                                debuggers. */
    IMP         method_imp;                   /* Address of the method in the 
                                                executable. */
  } method_list[1];                           /* Variable length 
                                                structure. */
} MethodList, *MethodList_t;

struct objc_protocol_list {
  struct objc_protocol_list *next;
  int count;
  Protocol *list[1];
};

/*
** This is used to assure consistent access to the info field of 
** classes
*/
#ifndef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG  (sizeof(long)*8)
#endif 

#define __CLS_INFO(cls) ((cls)->info)
#define __CLS_ISINFO(cls, mask) ((__CLS_INFO(cls)&mask)==mask)
#define __CLS_SETINFO(cls, mask) (__CLS_INFO(cls) |= mask)

/* The structure is of type MetaClass* */
#define _CLS_META 0x2L
#define CLS_ISMETA(cls) ((cls)&&__CLS_ISINFO(cls, _CLS_META))


/* The structure is of type Class* */
#define _CLS_CLASS 0x1L
#define CLS_ISCLASS(cls) ((cls)&&__CLS_ISINFO(cls, _CLS_CLASS))

/*
** The class is initialized within the runtime.  This means that 
** it has had correct super and sublinks assigned
*/
#define _CLS_RESOLV 0x8L
#define CLS_ISRESOLV(cls) __CLS_ISINFO(cls, _CLS_RESOLV)
#define CLS_SETRESOLV(cls) __CLS_SETINFO(cls, _CLS_RESOLV)

/*
** The class has been send a +initialize message or a such is not 
** defined for this class
*/
#define _CLS_INITIALIZED 0x04L
#define CLS_ISINITIALIZED(cls) __CLS_ISINFO(cls, _CLS_INITIALIZED)
#define CLS_SETINITIALIZED(cls) __CLS_SETINFO(cls, _CLS_INITIALIZED)

/*
** The class number of this class.  This must be the same for both the 
** class and it's meta class object
*/
#define CLS_GETNUMBER(cls) (__CLS_INFO(cls) >> (HOST_BITS_PER_LONG/2))
#define CLS_SETNUMBER(cls, num) \
  ({ (cls)->info <<= (HOST_BITS_PER_LONG/2); \
     (cls)->info >>= (HOST_BITS_PER_LONG/2); \
     __CLS_SETINFO(cls, (((unsigned long)num) << (HOST_BITS_PER_LONG/2))); })

/*
** The compiler generates one of these structures for each category.  A class
** may have many categories and contain both instance and factory methods.  
*/
typedef struct objc_category {
  const char*   category_name;                /* Name of the category.  Name
                                                contained in the () of the
                                                category definition. */
  const char*   class_name;                   /* Name of the class to which
                                                the category belongs. */
  MethodList_t  instance_methods;             /* Linked list of instance
                                                methods defined in the 
                                                category. NULL indicates no
                                                instance methods defined. */
  MethodList_t  class_methods;                /* Linked list of factory 
                                                methods defined in the
                                                category.  NULL indicates no
                                                class methods defined. */
  struct objc_protocol_list *protocols;	      /* List of Protocols 
					         conformed to */
} Category, *Category_t;

/*
** Structure used when a message is send to a class's super class.  The
** compiler generates one of these structures and passes it to
** objc_msg_super.
*/
typedef struct objc_super {
  id      self;                           /* Id of the object sending
                                                the message. */
  Class* class;                              /* Object's super class. */
} Super, *Super_t;

IMP objc_msg_lookup_super(Super_t super, SEL sel);

retval_t objc_msg_sendv(id, SEL, arglist_t);



/*
** This is a hook which is called by objc_lookup_class and
** objc_get_class if the runtime is not able to find the class.
** This may e.g. try to load in the class using dynamic loading.
** The function is guaranteed to be passed a non-NULL name string.
*/
extern Class* (*_objc_lookup_class)(const char *name);

extern id (*_objc_object_alloc)(Class* class);

extern id (*_objc_object_copy)(id object);

extern id (*_objc_object_dispose)(id object);

Method_t class_get_class_method(MetaClass* class, SEL aSel);

Method_t class_get_instance_method(Class* class, SEL aSel);

Class* class_pose_as(Class* impostor, Class* superclass);

Class* objc_get_class(const char *name);

Class* objc_lookup_class(const char *name);

Class* objc_next_class(void **enum_state);

const char *sel_get_name(SEL selector);

const char *sel_get_type(SEL selector);

SEL sel_get_uid(const char *name);

SEL sel_get_any_uid(const char *name);

SEL sel_get_typed_uid(const char *name, const char*);

SEL sel_register_name(const char *name);

SEL sel_register_typed_name(const char *name, const char*type);


BOOL sel_is_mapped (SEL aSel);

extern id class_create_instance(Class* class);

static inline const char *
class_get_class_name(Class* class)
{
  return CLS_ISCLASS(class)?class->name:((class==Nil)?"Nil":0);
}

static inline long
class_get_instance_size(Class* class)
{
  return CLS_ISCLASS(class)?class->instance_size:0;
}

static inline MetaClass*
class_get_meta_class(Class* class)
{
  return CLS_ISCLASS(class)?class->class_pointer:Nil;
}

static inline Class*
class_get_super_class(Class* class)
{
  return CLS_ISCLASS(class)?class->super_class:Nil;
}

static inline int
class_get_version(Class* class)
{
  return CLS_ISCLASS(class)?class->version:-1;
}

static inline BOOL
class_is_class(Class* class)
{
  return CLS_ISCLASS(class);
}

static inline BOOL
class_is_meta_class(Class* class)
{
  return CLS_ISMETA(class);
}


static inline void
class_set_version(Class* class, long version)
{
  if (CLS_ISCLASS(class))
    class->version = version;
}

static inline IMP
method_get_imp(Method_t method)
{
  return (method!=METHOD_NULL)?method->method_imp:(IMP)0;
}

IMP get_imp (Class* class, SEL sel);

id object_copy(id object);

id object_dispose(id object);

static inline Class*
object_get_class(id object)
{
  return ((object!=nil)
	  ? (CLS_ISCLASS(object->class_pointer)
	     ? object->class_pointer
	     : (CLS_ISMETA(object->class_pointer)
		? (Class*)object
		: Nil))
	  : Nil);
}

static inline const char *
object_get_class_name(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->name
                         :((Class*)object)->name)
                       :"Nil");
}

static inline MetaClass*
object_get_meta_class(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->class_pointer
                         :(CLS_ISMETA(object->class_pointer)
                           ?object->class_pointer
                           :Nil))
                       :Nil);
}

static inline Class*
object_get_super_class
(id object)
{
  return ((object!=nil)?(CLS_ISCLASS(object->class_pointer)
                         ?object->class_pointer->super_class
                         :(CLS_ISMETA(object->class_pointer)
                           ?((Class*)object)->super_class
                           :Nil))
                       :Nil);
}

static inline BOOL
object_is_class(id object)
{
  return CLS_ISCLASS((Class*)object);
}

static inline BOOL
object_is_instance(id object)
{
  return (object!=nil)&&CLS_ISCLASS(object->class_pointer);
}

static inline BOOL
object_is_meta_class(id object)
{
  return CLS_ISMETA((Class*)object);
}

#endif /* not __objc_api_INCLUDE_GNU */



