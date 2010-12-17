/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test nested 'dot syntax' (xxx.yyy.zzz or [xxx yyy].zzz).  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@class MyRootClass;

static MyRootClass *shared_root = nil;

@interface MyRootClass
{
  Class isa;
  int a;
  int b;
  MyRootClass *next;
}
@property int b;
@property (assign) MyRootClass *next;
+ (id) initialize;
+ (MyRootClass *)sharedInstance;
+ (id) alloc;
- (id) init;
- (MyRootClass *)same;
- (int) count;
- (void) setCount: (int)count;
@end

@implementation MyRootClass
@synthesize b;
@synthesize next;
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
+ (MyRootClass *)sharedInstance
{
  if (!shared_root)
    shared_root = [[self alloc] init];

  return shared_root;
}
- (MyRootClass *)same
{
  return self;
}
- (int) count
{
  return a;
}
- (void) setCount: (int)count
{
  a = count;
}
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  /* Test ClassName.accessor.accessor.  */
  MyRootClass.sharedInstance.count = 500;
  if (MyRootClass.sharedInstance.count != 500)
    abort ();

  /* Test object.accessor.accessor.  */
  object.same.count = 1000;
  if (object.same.count != 1000)
    abort ();

  /* Test object.accessor.property.  */
  object.same.next = object;
  if (object.same.next != object)
    abort ();

  /* Test lots of nesting.  */
  if (object.next.next.same.same.next.next.same != object)
    abort ();

  /* Test more nesting.  */
  MyRootClass.sharedInstance.next = object;
  MyRootClass.sharedInstance.next.next.next.next.next.count = 2000;
  if (MyRootClass.sharedInstance.next.next.next.next.next.count != 2000)
    abort ();

  /* Test more nesting.  */
  MyRootClass.sharedInstance.same.same.same.same.same.count = 3000;
  if (MyRootClass.sharedInstance.same.same.same.same.same.count != 3000)
    abort ();

  /* Test [object method].property.  */
  [MyRootClass sharedInstance].count = 5000;
  if ([MyRootClass sharedInstance].count != 5000)
    abort ();

  /* Just a final check.  */
  if (shared_root.count != 5000)
    abort ();

  return 0;
}


