/* Check @defs() in Objective-C++ */
/* Contributed by Devang Patel  <dpatel@apple.com>  */
/* { dg-options "" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdlib.h>
#include <objc/objc.h>

extern "C" void abort(void);

@interface A : TestsuiteObject
{
  @public
    int a;
}
@end

struct A_defs 
{
  @defs(A);
};

@implementation A
- init 
{
  a = 42;
  return self;
}
@end


int main() 
{
  A *a = [A init];
  struct A_defs *a_defs = (struct A_defs *)a;
  
  if (a->a != a_defs->a)
    abort ();	
  
  return 0;
}

