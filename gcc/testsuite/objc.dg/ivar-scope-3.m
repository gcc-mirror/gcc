/* Test instance variable scope.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do run } */
/* { dg-additional-options "-Wno-shadow-ivar" } */
#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <objc/objc.h>

extern void abort(void);

int someivar = 1;

@interface MyClass: TestsuiteObject
{
  int someivar;
}
- (int) get;
- (int) getHidden;
@end

@implementation MyClass
- init
{
  someivar = 2;

  return self;
}

- (int) get
{
  return someivar;
}

- (int) getHidden
{
  int someivar = 3;
  
  return someivar;
}
@end

int main(void)
{
  MyClass *object;

  object = [[MyClass alloc] init];

  /* Check whether the instance variable hides the global variable. */
  
  if ([object get] != 2) {
    abort();
  }

  /* Check whether the local variable hides the instance variable. */
  
  if ([object getHidden] != 3) {
    abort();
  }

  return 0;
}
