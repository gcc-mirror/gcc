/* Test instance variable scope.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do run } */
/* { dg-additional-options "-Wno-shadow-ivar -fno-local-ivars" } */
#include "../objc-obj-c++-shared/runtime.h"
#include <objc/objc.h>

extern void abort(void);

int someivar = 1;

/* The testsuite object depends on local variable scope so we need to
   implement our own minimal base object here. */

@interface MyClass
{
  Class isa;
  int someivar;
}

+ (id) initialize;
+ (id) alloc;
- (id) init;
- (int) getGlobal;
- (int) getInstance;
- (int) getHidden;
@end

@implementation MyClass
+ (id) initialize
{
  return self;
}

+ (id) alloc
{
  return class_createInstance (self, 0);
}

- (id) init
{
  self->someivar = 2;

  return self;
}

- (int) getGlobal
{
  return someivar;
}

- (int) getInstance
{
  return self->someivar;
}

- (int) getHidden
{
  int someivar = 3;
  
  return someivar;
}
@end

int main(void)
{
  id object;

  object = [[MyClass alloc] init];

  /* Check for aliasing between instance variable and global
     variable. */

  if ([object getGlobal] != 1) {
    abort();
  }
  
  if ([object getInstance] != 2) {
    abort();
  }

  /* Check whether the local variable hides the instance variable. */
  
  if ([object getHidden] != 3) {
    abort();
  }

  return 0;
}
