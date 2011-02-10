/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-1.h"

@implementation TestClass1
+ initialize { return self; }
+ load
{
  increase_load_count ();
}
@end

@implementation TestClass2 (Category)
+ load
{
  increase_load_count ();
}
@end


static int load_count = 0;

int increase_load_count (void)
{
  load_count++;
}

int main (void)
{
  if (load_count != 4)
    abort ();

  return 0;
}
