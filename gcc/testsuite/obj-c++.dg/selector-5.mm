/* { dg-options "" } */
/* { dg-do compile } */

#include <objc/objc.h>

int main()
{
  SEL foo = @selector(foo::);
  return 0;
}

/* { dg-final { scan-assembler  "foo::" } } */

