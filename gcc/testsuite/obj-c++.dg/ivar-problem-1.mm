/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* This test checks what happens if there are 16 instance variables.
   In that case, the class was not created correctly.  In this testcase,
   we have two classes, one with 15 variables and one with 16.  Older
   GCCs would generate a bogus warning for the second class but not 
   for the first one.  This only happened for ObjC, but it's good to
   test ObjC++ as well.  */

#include <stdlib.h>
#include <objc/objc.h>

@interface MyRootClass1
{
  Class isa;
  int v2;
  int v3;
  int v4;
  int v5;
  int v6;
  int v7;
  int v8;
  int v9;
  int v10;
  int v11;
  int v12;
  int v13;
  int v14;
  int v15;
}
- (id) init;
@end

@implementation MyRootClass1
- (id) init { return self; }
@end


@interface MyRootClass2
{
  Class isa;
  int v2;
  int v3;
  int v4;
  int v5;
  int v6;
  int v7;
  int v8;
  int v9;
  int v10;
  int v11;
  int v12;
  int v13;
  int v14;
  int v15;
  /* Adding the 16th variable used to cause bogus warnings to be
     generated.  */
  int v16;
}
- (id) init;
@end

@implementation MyRootClass2
- (id) init { return self; } /* This should not generate a bogus warning.  */
@end
