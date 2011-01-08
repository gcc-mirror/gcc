#ifndef _OBJC_NEXT_MAPPING_H_
#define _OBJC_NEXT_MAPPING_H_

/* This file "renames" various ObjC GNU runtime entry points
   (and fakes the existence of several others)
   if the NeXT runtime is being used.  */
/* Authors: Ziemowit Laski <zlaski@apple.com>  */
/*	    David Ayers <d.ayers@inode.at>  */
/* Darwin 64bit/OBJC2 modifications Iain Sandoe */ 

#ifndef __NEXT_RUNTIME__

#define CLASSPTRFIELD(x) (x)->class_pointer

#else
/* Includes next-abi.h to set NEXT_OBJC_USE_NEW_INTERFACE etc.*/
#ifndef _OBJC_OBJECT1_H_
#include "Object1.h"
#endif
#include <objc/objc-class.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Force a definition of nil that is compatible with GNU runtime.  */
#undef  nil
#define nil ((id)0)

#define objc_get_class(C) objc_getClass(C)
#define objc_get_meta_class(C) objc_getMetaClass(C)
#define class_get_class_method(C, S) class_getClassMethod(C, S)
#define class_get_instance_method(C, S) class_getInstanceMethod(C, S)
#define sel_get_name(S) sel_getName(S)
#define class_create_instance(C) class_createInstance(C, 0)
#define	class_get_class_name(C) object_getClassName(C)

#define CLASSPTRFIELD(x) (x)->isa

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
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

#else
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
#endif

#define objc_lookup_class(N) objc_lookUpClass(N)

/* You need either an empty +initialize method or an empty -forward:: method. 
   The NeXT runtime unconditionally sends +initialize to classes when they are 
   first used, and unconditionally tries to forward methods that the class 
   doesn't understand (including +initialize). If you have neither +initialize 
   nor -forward::, the runtime complains.  

   The simplest workaround is to add

      + initialize { return self; }

   to every root class @implementation.  */

#ifndef NULL
#define NULL 0
#endif


/* A small, portable NSConstantString implementation for use with the NeXT
   runtime.
   
   On full-fledged Mac OS X systems, NSConstantString is provided
   as part of the Foundation framework.  However, on bare Darwin systems,
   Foundation is not included, and hence there is no NSConstantString 
   implementation to link against.

   This code is derived from the GNU runtime's NXConstantString implementation.
*/

/* This definition cut out of <objc/Object.h> with the OBJC2 deprecation
   messages removed. 
*/
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
struct fudge_objc_class {
    Class isa;
#if NEXT_OBJC_ABI_VERSION < 2
    Class super_class ;
    const char *name ;
    long version  ;
    long info ;
    long instance_size ;
    struct anon *ivars ; /* objc_ivar_list */
    struct anon1 **methodLists ; /* objc_method_list */
    struct objc_cache *cache  ;
    struct objc_protocol_list *protocols ;
#endif
} _NSConstantStringClassReference ;
#else
struct objc_class _NSConstantStringClassReference ;
#endif

@interface NSConstantString : Object
{
  char *c_string;
  unsigned int len;
}

-(const char *) cString;
-(unsigned int) length;

@end

@implementation NSConstantString

-(const char *) cString
{
  return (c_string);
}

-(unsigned int) length
{
  return (len);
}

@end

/* The NSConstantString metaclass will need to be initialized before we can
   send messages to strings.  */

void objc_constant_string_init (void) __attribute__((constructor));
void objc_constant_string_init (void) {
  memcpy (&_NSConstantStringClassReference,
	  objc_getClass ("NSConstantString"),
	  sizeof (_NSConstantStringClassReference));
}

#endif  /*__NEXT_RUNTIME__ */
#endif /* _OBJC_NEXT_MAPPING_H_ */