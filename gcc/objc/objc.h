/* Basic data types for Objective C.
   Copyright (C) 1993 Free Software Foundation, Inc.

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


/*
** Hash-cache or sparse arrays?
*/ 
#define OBJC_SPARSE_LOOKUP 	/* use sparse-arrays for lookup */
/* #define OBJC_HASH_LOOKUP */ /* use hash-cache for lookup */

#ifdef OBJC_SPARSE_LOOKUP
extern const char* __objc_sparse_lookup_id;
#endif

#ifdef OBJC_HASH_LOOKUP
extern const char* __objc_hash_lookup_id;
#endif


#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>


#define nil (id)0                               /* id of Nil instance */
#define Nil (Class_t)0                          /* id of Nil class */
typedef char *STR;                              /* String alias */

                                                /* Boolean typedefs */
typedef char  BOOL;
#define YES   (BOOL)1
#define NO    (BOOL)0


/* For functions which return Method_t */
#define METHOD_NULL	(Method_t)0



/*
** Definition of a selector.  Selectors are really of type unsigned int.
** The runtime does this mapping from SEL's to names internally in the
** sel_... operations.  You should never use the fact that it is actually
** an integer, since other Objective-C implementations use other conventions.
*/
typedef void* SEL;

/*
** ObjC uses this typedef for untyped instances.
*/
typedef struct objc_object {
  struct objc_class*  class_pointer;
} *id;

typedef id (*IMP)(id, SEL, ...); 

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
** message that is sent.
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
    int         ivar_offset;                    /* Byte offset from the base 
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
  int             method_count;               /* Number of methods defined in 
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


#include <objc/sarray.h>

#ifdef OBJC_HASH_LOOKUP

/*
** This data structure is used for the hash lookup mechanism.  When
** enabled, the runtime keeps a such cache of buckets for each class.
*/
typedef struct objc_cache* Cache_t;
typedef struct objc_bucket* Bucket_t;
typedef struct objc_bucket Bucket;
struct objc_cache {
  unsigned int mask;		/* total = mask+1 */
  unsigned int occupied;
  struct objc_bucket {
    SEL method_selector;
    IMP method_imp;
  } buckets[1];
} Cache;

#endif

/*
** The compiler generates one of these structures for each class.  
**
** This structure is the definition for meta classes. By definition a meta
** class is the class's class.  Its most relevant contribution is that its
** method list contain the class's factory methods. 
**
** This structure is generated by the compiler in the executable and used by
** the run-time during normal messaging operations.  Therefore some members
** change type. The compiler generates "char* const" and places a string in
** the following member variables:  class_pointer and super_class. 
*/
typedef struct objc_class *MetaClass_t;

/*
** The compiler generates one of these structures for each class.  
** 
** This structure is the definition for classes. 
** 
** This structure is generated by the compiler in the executable and used by
** the run-time during normal messaging operations.  Therefore some members
** change type. The compiler generates "char* const" and places a string in
** the following member variables:  super_class. 
*/
struct objc_class {     
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
#ifdef OBJC_HASH_LOOKUP
  Cache_t	     cache;                   /* Pointer to method cache */
#else
  struct sarray *    dtable;                  /* Pointer to instance 
					         method dispatch table. */  
#endif

  struct objc_class* subclass_list;           /* Subclasses */
  struct objc_class* sibling_class;

  struct objc_protocol_list *protocols;	      /* Protocols conformed to */

};
#define Class struct objc_class
#define Class_t Class*
typedef struct objc_class MetaClass;

/* Protocol support */

#ifndef __OBJC__
typedef struct objc_protocol {
  char *protocol_name;
  struct objc_protocol_list *protocol_list;
  struct objc_method_description_list *instance_methods, *class_methods; 
} Protocol; 

#else

@class Protocol;
#endif 

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

/* The structure is of type MetaClass_t */
#define _CLS_META 0x2L
#define CLS_ISMETA(cls) ((cls)&&__CLS_ISINFO(cls, _CLS_META))


/* The structure is of type Class_t */
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
  ({ assert(CLS_GETNUMBER(cls)==0); \
     __CLS_SETINFO(cls, ((num) << (HOST_BITS_PER_LONG/2))); })

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
** Well...
*/

typedef struct objc_typed_stream TypedStream;
#include <objc/objc-archive.h>


/*
** Structure used when a message is send to a class's super class.  The
** compiler generates one of these structures and passes it to
** objc_msg_super.
*/
typedef struct objc_super {
  id      self;                           /* Id of the object sending
                                                the message. */
  Class_t class;                              /* Object's super class. */
} Super, *Super_t;

IMP objc_msg_lookup_super(Super_t super, SEL sel);

typedef void* retval_t;		/* return value */
typedef void(*apply_t)(void);	/* function pointer */

#if defined(REG_ARGS) || defined(STACK_ARGS)

typedef struct {
  char* arg_pointer;
#ifdef STRUCT_RETURN
  void* struct_return;
#endif
#ifdef REG_ARGS
  void* regs[2];
#endif
} *arglist_t;

#ifdef REG_ARGS
#define __objc_frame_receiver(FRAME)  (FRAME)->regs[0]
#define __objc_frame_selector(FRAME)  ((SEL)(FRAME)->regs[1])

#else
#define __objc_frame_receiver(FRAME) ((id*)(FRAME)->arg_pointer)[0]
#define __objc_frame_selector(FRAME) ((SEL*)(FRAME)->arg_pointer)[1]
#endif
#else

typedef void* arglist_t;

#endif

retval_t objc_msg_sendv(id, SEL, size_t, arglist_t);

#ifdef __OBJC__

static id nil_method(id rcv, SEL op, ...) { return rcv; }

#ifdef OBJC_HASH_LOOKUP

#include <objc/cache.h>

extern __inline__ IMP
objc_msg_lookup(id receiver, SEL op)
{
  if(receiver)
    return cache_get(receiver->class_pointer, op);
  else
    return nil_method;
}

#else /* not OBJC_HASH_LOOKUP => OBJC_SPARSE_LOOKUP */

extern __inline__ IMP
objc_msg_lookup(id receiver, SEL op)
{
  if(receiver)
    return sarray_get(receiver->class_pointer->dtable, (unsigned int) op);
  else
    return nil_method;
}
#endif /* not OBJC_HASH_LOOKUP */

#else
 IMP objc_msg_lookup(id, SEL);
#endif


#endif /* not __objc_INCLUDE_GNU */
