/* Test basic nested C function functionality within ObjC
   methods.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-lobjc" } */
/* { dg-do run } */
#include <stdio.h>
#include <objc/objc.h>
#include <objc/Object.h>

int bappy (int (*blargh) (int a, int b, int c))
{
  return blargh (4, 7, 2) + 3;
}

@interface Foo: Object
+ (int)foo;
@end

@implementation Foo
+ (int)foo
{
  int blargh (int a, int b, int c)
  {
    return a * b + c;
  }
  return bappy (blargh);
}
@end

int main () 
{
  int f = [Foo foo];
  if (f != 33)
    abort ();

  return 0;
}
