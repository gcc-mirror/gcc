/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test warnings and non-warnings with @optional @properties.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@end

@protocol count
@optional
@property int count1;
@property (readonly) int count2;
@end


/* A class that implements all the properties.  */
@interface MySubClass1 : MyRootClass <count>
{
  int count1;
  int count2;
}
@end

@implementation MySubClass1
@synthesize count1;
@synthesize count2;
@end


/* A class that implements nothing; no warnings as the properties are
   all optional.  */
@interface MySubClass2 : MyRootClass <count>
@end

@implementation MySubClass2
@end


@protocol count2
@required
@property int count1;
@property (readonly) int count2;
@end

/* A class that implements all the properties.  */
@interface MySubClass3 : MyRootClass <count2>
{
  int count1;
  int count2;
}
@end

@implementation MySubClass3
@synthesize count1;
@synthesize count2;
@end


/* A class that implements nothing; warnings as the properties are
   all required.  */
@interface MySubClass4 : MyRootClass <count2>
@end

@implementation MySubClass4
@end
/* { dg-warning "incomplete implementation of class" "" { target *-*-* } .-1 } */
/* { dg-warning "method definition for ..setCount1:. not found" "" { target *-*-* } .-2 } */
/* { dg-warning "method definition for ..count1. not found" "" { target *-*-* } .-3 } */
/* { dg-warning "method definition for ..count2. not found" "" { target *-*-* } .-4 } */
/* { dg-warning "class .MySubClass4. does not fully implement the .count2. protocol" "" { target *-*-* } .-5 } */
