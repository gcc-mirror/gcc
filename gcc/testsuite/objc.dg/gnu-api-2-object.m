/* Test the Modern GNU Objective-C Runtime API.

  This is test 'object', covering all functions starting with 'object'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
+ initialize;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
+ initialize { return self; }
@end

@protocol MyProtocol
- (id) variable;
@end

@protocol MySecondProtocol
- (id) setVariable: (id)value;
@end

@interface MySubClass : MyRootClass <MyProtocol>
{ id variable_ivar; }
- (void) setVariable: (id)value;
- (id) variable;
@end

@implementation MySubClass
- (void) setVariable: (id)value { variable_ivar = value; }
- (id) variable { return variable_ivar; }
@end

@interface MySubSubClass : MySubClass
- (id) test;
@end

@implementation MySubSubClass
- (id) test { return self; }
@end



int main(int argc, void **args)
{
  /* Functions are tested in alphabetical order.  */
  
  printf ("Testing object_copy () ...\n");
  {
    MySubClass *object_a = [[MySubClass alloc] init];
    MySubClass *object_b = object_copy (object_a, 0);

    [object_b setVariable: object_a];
    if ([object_b variable] != object_a)
      abort ();
  }

  printf ("Testing object_dispose () ...\n");
  {
    MySubClass *object = [[MySubClass alloc] init];

    object_dispose (object);
  }

  printf ("Testing object_getClass () ...\n");
  {
    MyRootClass *o = [[MySubClass alloc] init];

    if (object_getClass (o) != objc_getClass ("MySubClass"))
      abort ();
  }

  printf ("Testing object_getClassName () ...\n");
  {
    MyRootClass *o = [[MyRootClass alloc] init];

    if (strcmp (object_getClassName (o), "MyRootClass") != 0)
      abort ();
  }

  printf ("Testing object_getIndexedIvars () ...\n");
  {
    if (object_getIndexedIvars ([[MyRootClass alloc] init]) == NULL)
      abort ();
  }
  
  printf ("Testing object_getInstanceVariable () ...\n");
  {
    MySubClass *o = [[MySubClass alloc] init];
    id value;

    [o setVariable: o];

    if (object_getInstanceVariable (o, "variable_ivar", (void **)&value) == NULL)
      abort ();

    if (value != o)
      abort ();
  }

  printf ("Testing object_getIvar () ...\n");
  {
    MySubClass *o = [[MySubClass alloc] init];
    Ivar ivar = class_getInstanceVariable (objc_getClass ("MySubClass"), "variable_ivar");

    [o setVariable: o];

    if (object_getIvar (o, ivar) != o)
      abort ();
  }

  printf ("Testing object_setClass () ...\n");
  {
    MySubClass *o = [[MySubClass alloc] init];

    object_setClass (o, objc_getClass ("MySubSubClass"));

    if ([(MySubSubClass *)o test] != o)
      abort ();
  }

  printf ("Testing object_setInstanceVariable () ...\n");
  {
    MySubClass *o = [[MySubClass alloc] init];
    
    [o setVariable: nil];

    if (object_setInstanceVariable (o, "variable_ivar", (void *)o) == NULL)
      abort ();

    if ([o variable] != o)
      abort ();
  }

  printf ("Testing object_setIvar () ...\n");
  {
    MySubClass *o = [[MySubClass alloc] init];
    MySubClass *value = [[MySubClass alloc] init];
    Ivar ivar = class_getInstanceVariable (objc_getClass ("MySubClass"), "variable_ivar");
    
    [o setVariable: o];

    object_setIvar (o, ivar, value);

    if ([o variable] != value)
      abort ();
  }  

  return 0;
}
