/* Basic data types for Objective C.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */
 

#ifndef __objc_INCLUDE_GNU
#define __objc_INCLUDE_GNU

/* If someone is using a c++ compiler then adjust the types in the
   file back to C.  */
#ifdef __cplusplus
extern "C" {
#endif

#include  "record.h"


#define nil (id)0                               /* id of Nil instance */
#define Nil (Class_t)0                          /* id of Nil class */
typedef char* STR;                              /* String alias */

                                                /* Boolean typedefs */
typedef char  BOOL;
#define YES   (BOOL)1
#define NO    (BOOL)0

/* Definition of a selector.  Selectors are really of type char*. The
   run-time hashes the string's address to locate the method.  If the
   method isn't in the hash table then a search is made through the
   class hierarchy using strcmp to locate the method.  */
#if 0
typedef struct objc_selector*   SEL;
#else
typedef void* SEL;
#endif

/* ObjC uses this typedef for untyped instances.  */

typedef struct objc_object {
  struct objc_class*  class_pointer;
} *id;

/* Prototype for method functions. */
typedef id  (*IMP)(id, SEL, ...); 

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
#define _C_ARY_B    '['
#define _C_ARY_E    ']'
#define _C_UNION_B  '('
#define _C_UNION_E  ')'
#define _C_STRUCT_B '{'
#define _C_STRUCT_E '}'

/* These definitions are masks used with the "info" member variable in
   the lass and meta class structures.  */
#define CLS_CLASS         0x1L                  /* The structure is of type
                                                  class (Class_t). */
#define CLS_META          0x2L                  /* The structure is of type
                                                  meta class (MetaClass_t). */
#define CLS_INITIALIZED   0x4L                  /* Class is initialized. A
                                                  +initialize method is the
                                                  first message sent to a
                                                  class.  It isn't guaranteed
                                                  to be sent only once. */
#define CLS_RTI           0x8L                  /* The class has been initialized
						   within the run time library. */

/* Set this variable nonzero to print a line describing each
   message that is sent.  */
extern BOOL objc_trace;


/*
 * Whereas a Module (defined further down) is the root (typically) of a file,
 * a Symtab is the root of the class and category definitions within the
 * module.  
 *
 * A Symtab contains a variable length array of pointers to classes and
 * categories  defined in the module. 
 */
typedef struct objc_symtab {
  unsigned long sel_ref_cnt;                     /* Unknown. */
  SEL       *refs;                              /* Unknown. */
  unsigned short cls_def_cnt;                   /* Number of classes compiled
                                                  (defined) in the module. */
  unsigned short cat_def_cnt;                   /* Number of categories 
                                                  compiled (defined) in the 
                                                  module. */
  void      *defs[1];                           /* Variable array of pointers.
                                                  cls_def_cnt of type Class_t 
                                                  followed by cat_def_cnt of
                                                  type Category_t. */
} Symtab,   *Symtab_t;


/*
 * The compiler generates one of these structures for each module that
 * composes the executable (eg main.m).  
 *
 * This data structure is the root of the definition tree for the module.  
 *
 * A collect program runs between ld stages and creates a ObjC ctor array. 
 * That array holds a pointer to each module structure of the executable. 
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
 * The compiler generates one of these structures for a class that has
 * instance variables defined in its specification. 
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
    int         ivar_offset;                    /* Byte offset from the base 
                                                  address of the instance 
                                                  structure to the variable. */

  } ivar_list[1];                               /* Variable length 
                                                  structure. */
} IvarList, *IvarList_t;


/*
 * The compiler generates one (or more) of these structures for a class that
 * has methods defined in its specification. 
 *
 * The implementation of a class can be broken into separate pieces in a file
 * and categories can break them across modules. To handle this problem is a
 * singly linked list of methods. 
 */
typedef struct objc_method Method;
typedef Method* Method_t;
typedef struct objc_method_list {
  struct objc_method_list*  method_next;      /* This variable is used to link 
                                                a method list to another.  It 
                                                is a singly linked list. */
  int             method_count;               /* Number of methods defined in 
                                                this structure. */
  struct objc_method {
    SEL         method_name;                  /* This variable is the method's 
                                                name.  It is a char*. 
                                                  The unique integer passed to 
                                                objc_msgSend is a char* too.  
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


/*
 * The compiler generates one of these structures for each class.  
 *
 * This structure is the definition for meta classes. By definition a meta
 * class is the class's class.  Its most relevant contribution is that its
 * method list contain the class's factory methods. 
 *
 * This structure is generated by the compiler in the executable and used by
 * the run-time during normal messaging operations.  Therefore some members
 * change type. The compiler generates "char* const" and places a string in
 * the following member variables:  class_pointer and super_class. 
 */
typedef struct objc_metaClass {     
  struct objc_metaClass*  class_pointer;      /* Pointer to Object meta class. */
  struct objc_metaClass*  super_class;        /* Pointer to meta class's
                                                super class. NULL for 
                                                Object. */
  const char*             name;               /* Name of the meta class. */
  long                    version;            /* Unknown. */
  long                    info;               /* Bit mask.  See class masks 
                                                defined above. */
  long                    instance_size;      /* Always 0 except for Object.
                                                Should be ignored. */
  IvarList_t              ivars;              /* Always NULL except for 
                                                Object.  Should be ignored. */
  MethodList_t            methods;            /* Linked List of factory methods 
                                                for the class. */
  struct record **        cache;              /* Pointer to factory method dispatch table. */
} MetaClass, *MetaClass_t;


/*
 * The compiler generates one of these structures for each class.  
 *
 * This structure is the definition for classes. 
 *
 * This structure is generated by the compiler in the executable and used by
 * the run-time during normal messaging operations.  Therefore some members
 * change type. The compiler generates "char* const" and places a string in
 * the following member variables:  super_class. 
 */
typedef struct objc_class {     
  MetaClass_t         class_pointer;          /* Pointer to the class's
                                                meta class. */
  struct objc_class*  super_class;            /* Pointer to the super 
                                                class. NULL for class 
                                                Object. */
  const char*         name;                   /* Name of the class. */
  long                version;                /* Unknown. */
  long                info;                   /* Bit mask.  See class masks 
                                                defined above. */
  long                instance_size;          /* Size in bytes of the class.  
                                                The sum of the class definition 
                                                and all super class 
                                                definitions. */
  IvarList_t          ivars;                  /* Pointer to a structure that
                                                describes the instance 
                                                variables in the class
                                                definition.  NULL indicates
                                                no instance variables.  Does
                                                not include super class
                                                variables. */
  MethodList_t        methods;                /* Linked list of instance
                                                methods defined for the 
                                                class. */
  struct record **    cache;                  /* Pointer to instance method dispatch table. */
} Class, *Class_t;


/*
 * The compiler generates one of these structures for each category.  A class
 * may have many categories and contain both instance and factory methods.  
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
} Category, *Category_t;


/*
 * Structure used when a message is send to a class's super class.  The
 * compiler generates one of these structures and passes it to
 * objc_msgSuper. 
 */
typedef struct objc_super {
  id      receiver;                           /* Id of the object sending
                                                the message. */
  Class_t class;                              /* Object's super class. */
} Super, *Super_t;

/*
 * _alloc points to the function, called through class_createInstance, used
 * to allocate memory for new instances. 
 */
extern id (*_alloc)(Class_t);
/*
 * _dealloc points to the function, called through object_dispose, used to
 * free instances. 
 */
extern id (*_dealloc)(id);
/*
 * _realloc points to the function, called through object_realloc, used to
 * reallocate memory for an object 
 */
extern id (*_realloc)(id, unsigned int);

/*
 * _copy points to the function, called through object_copy, used to create
 * an exact copy of an object. 
 */
extern  id (*_copy)(id);

/*
 * _error points to the function that the run-time system calls in response
 * to an error.  By default, it prints formatted error messages to the
 * standard error stream and calls abort to produce a core file. 
 */
extern void (*_error)(id object, const char *fmt, va_list ap);


#ifdef __cplusplus
}
#endif


#endif /* not __objc_INCLUDE_GNU */
