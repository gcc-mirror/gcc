/* Test basic nested C function functionality within ObjC
   methods.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
#include <stdio.h>
#include <stdlib.h>
#import "../../objc-obj-c++-shared/TestsuiteObject.m"
#include <objc/objc.h>

int bappy (int (*blargh) (int a, int b, int c))
{
  return blargh (4, 7, 2) + 3;
}

@interface Foo: TestsuiteObject
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
