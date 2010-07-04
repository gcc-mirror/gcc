/* { dg-options "" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"

int main()
{
  SEL foo = @selector(foo: a::);
  return 0;
}

/* { dg-final { scan-assembler  "foo:a::" } } */

