/* Check if casting 'self' or 'super' affects message lookup in the
   correct way.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

#include <stddef.h>
#include <objc/objc.h>
#include <objc/Object.h>

#ifdef __NEXT_RUNTIME__
#define OBJC_GETCLASS objc_getClass
#else
#define OBJC_GETCLASS objc_get_class
#endif

@protocol Func
+ (int) class_func0;
- (int) instance_func0;
@end

@interface Derived: Object
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
   return i + (size_t)[super class_func0];   /* { dg-warning ".Object. may not respond to .\\+class_func0." } */
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
   return [(Object <Func> *)super class_func0];
}
+ (int) class_func4
{
   return [(Derived <Func> *)super class_func0];
}   
+ (int) class_func5
{
   int i = (size_t)[Derived class_func0];    /* { dg-warning ".Derived. may not respond to .\\+class_func0." } */
   return i + (size_t)[Object class_func0];  /* { dg-warning ".Object. may not respond to .\\+class_func0." } */
}
+ (int) class_func6
{
   return (size_t)[OBJC_GETCLASS("Object") class_func1];  /* { dg-warning ".Object. may not respond to .\\+class_func1." } */
}
+ (int) class_func7
{
   return [OBJC_GETCLASS("Derived") class_func1];
}
- (int) instance_func1
{
   int i = (size_t)[self instance_func0];     /* { dg-warning ".Derived. may not respond to .\\-instance_func0." } */
   return i + (size_t)[super instance_func0]; /* { dg-warning ".Object. may not respond to .\\-instance_func0." } */
}
- (int) instance_func2
{
   return [(id <Func>)super instance_func0];
}
- (int) instance_func3
{
   return [(Object <Func> *)super instance_func0];
}
- (int) instance_func4
{
   return [(Derived <Func> *)super instance_func0];
}   
- (int) instance_func5
{
   int i = (size_t)[Derived instance_func1]; /* { dg-warning ".Derived. may not respond to .\\+instance_func1." } */
   return i + (size_t)[Object instance_func1]; /* { dg-warning ".Object. may not respond to .\\+instance_func1." } */
}
- (int) instance_func6
{
   return (size_t)[OBJC_GETCLASS("Object") class_func1]; /* { dg-warning ".Object. may not respond to .\\+class_func1." } */
}
- (int) instance_func7
{
   return [OBJC_GETCLASS("Derived") class_func1];
}
@end

@implementation Derived (Categ)
+ (int) categ_class_func1
{
   int i = (size_t)[self class_func0];       /* { dg-warning ".Derived. may not respond to .\\+class_func0." } */
   i += [self class_func1];
   i += [self categ_class_func2];
   i += (size_t)[self categ_instance_func1]; /* { dg-warning ".Derived. may not respond to .\\+categ_instance_func1." } */
   return i + (size_t)[super class_func0];   /* { dg-warning ".Object. may not respond to .\\+class_func0." } */
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
   i += (size_t)[(Object <Func> *)self categ_instance_func2]; /* { dg-warning ".Object. may not respond to .\\-categ_instance_func2." } */
   /* { dg-warning ".\\-categ_instance_func2. not found in protocol" "" { target *-*-* } 131 } */
   i += (size_t)[(id <Func>)self categ_instance_func2];  /* { dg-warning ".\\-categ_instance_func2. not found in protocol" } */
   i += [(id)self categ_instance_func2];
   return i + (size_t)[super instance_func0];   /* { dg-warning ".Object. may not respond to .\\-instance_func0." } */
}
- (int) categ_instance_func2
{
   return [(id <Func>)super instance_func0];
}
@end

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */
