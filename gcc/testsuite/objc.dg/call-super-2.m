/* Check if casting 'self' or 'super' affects message lookup in the correct way.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include "../objc-obj-c++-shared/runtime.h"
#include <stddef.h>

/* FIXME: This is temporary.  At the moment, the compiler, when
   compiling for the GNU runtime and doing method checks, only
   recognizes objc_get_class(), and not objc_getClass().  So
   temporarily force objc_get_class() to be used.  */
#ifndef __NEXT_RUNTIME__
# define objc_getClass(C) objc_get_class(C)
#endif

@protocol Func
+ (int) class_func0;
- (int) instance_func0;
@end

@interface Derived: TestsuiteObject
+ (int) class_func1;
+ (int) class_func2;
+ (int) class_func3;
+ (int) class_func4;
+ (int) class_func5;
+ (int) class_func6;
+ (int) class_func7;
- (int) instance_func1;
- (int) instance_func2;
- (int) instance_func3;
- (int) instance_func4;
- (int) instance_func5;
- (int) instance_func6;
- (int) instance_func7;
@end

@interface Derived (Categ)
+ (int) categ_class_func1;
+ (int) categ_class_func2;
- (int) categ_instance_func1;
- (int) categ_instance_func2;
@end

@implementation Derived
+ (int) class_func1
{
   int i = (size_t)[self class_func0];       /* { dg-warning ".Derived. may not respond to .\\+class_func0." } */
   return i + (size_t)[super class_func0];   /* { dg-warning ".TestsuiteObject. may not respond to .\\+class_func0." } */
}
+ (int) class_func2
{
   int i = [(id <Func>)self class_func0];  /* { dg-warning ".\\-class_func0. not found in protocol" } */
   i += [(id <Func>)super class_func0];    /* { dg-warning ".\\-class_func0. not found in protocol" } */
   i += [(Class <Func>)self class_func0];
   return i + [(Class <Func>)super class_func0];
}
+ (int) class_func3
{
   return [(TestsuiteObject <Func> *)super class_func0];
}
+ (int) class_func4
{
   return [(Derived <Func> *)super class_func0];
}   
+ (int) class_func5
{
   int i = (size_t)[Derived class_func0];    /* { dg-warning ".Derived. may not respond to .\\+class_func0." } */
   return i + (size_t)[TestsuiteObject class_func0];  /* { dg-warning ".TestsuiteObject. may not respond to .\\+class_func0." } */
}
+ (int) class_func6
{
   return (size_t)[objc_getClass("TestsuiteObject") class_func1];  /* { dg-warning ".TestsuiteObject. may not respond to .\\+class_func1." } */
}
+ (int) class_func7
{
   return [objc_getClass("Derived") class_func1];
}
- (int) instance_func1
{
   int i = (size_t)[self instance_func0];     /* { dg-warning ".Derived. may not respond to .\\-instance_func0." } */
   return i + (size_t)[super instance_func0]; /* { dg-warning ".TestsuiteObject. may not respond to .\\-instance_func0." } */
}
- (int) instance_func2
{
   return [(id <Func>)super instance_func0];
}
- (int) instance_func3
{
   return [(TestsuiteObject <Func> *)super instance_func0];
}
- (int) instance_func4
{
   return [(Derived <Func> *)super instance_func0];
}   
- (int) instance_func5
{
   int i = (size_t)[Derived instance_func1]; /* { dg-warning ".Derived. may not respond to .\\+instance_func1." } */
   return i + (size_t)[TestsuiteObject instance_func1]; /* { dg-warning ".TestsuiteObject. may not respond to .\\+instance_func1." } */
}
- (int) instance_func6
{
   return (size_t)[objc_getClass("TestsuiteObject") class_func1]; /* { dg-warning ".TestsuiteObject. may not respond to .\\+class_func1." } */
}
- (int) instance_func7
{
   return [objc_getClass("Derived") class_func1];
}
@end

@implementation Derived (Categ)
+ (int) categ_class_func1
{
   int i = (size_t)[self class_func0];       /* { dg-warning ".Derived. may not respond to .\\+class_func0." } */
   i += [self class_func1];
   i += [self categ_class_func2];
   i += (size_t)[self categ_instance_func1]; /* { dg-warning ".Derived. may not respond to .\\+categ_instance_func1." } */
   return i + (size_t)[super class_func0];   /* { dg-warning ".TestsuiteObject. may not respond to .\\+class_func0." } */
}
+ (int) categ_class_func2
{
   int i = [(id <Func>)self class_func0];  /* { dg-warning ".\\-class_func0. not found in protocol" } */
   i += [(id <Func>)super class_func0];    /* { dg-warning ".\\-class_func0. not found in protocol" } */
   i += [(Class <Func>)self class_func0];
   return i + [(Class <Func>)super class_func0];
}
- (int) categ_instance_func1
{
   int i = (size_t)[self instance_func0];    /* { dg-warning ".Derived. may not respond to .\\-instance_func0." } */
   i += [(Derived <Func> *)self categ_instance_func2];
   i += (size_t)[(TestsuiteObject <Func> *)self categ_instance_func2]; /* { dg-warning ".TestsuiteObject. may not respond to .\\-categ_instance_func2." } */
   /* { dg-warning ".\\-categ_instance_func2. not found in protocol" "" { target *-*-* } .-1 } */
   i += (size_t)[(id <Func>)self categ_instance_func2];  /* { dg-warning ".\\-categ_instance_func2. not found in protocol" } */
   i += [(id)self categ_instance_func2];
   return i + (size_t)[super instance_func0];   /* { dg-warning ".TestsuiteObject. may not respond to .\\-instance_func0." } */
}
- (int) categ_instance_func2
{
   return [(id <Func>)super instance_func0];
}
@end

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */
