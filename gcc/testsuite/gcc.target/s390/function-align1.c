/* { dg-do run } */
/* { dg-options "-O2 -march=z13"  } */

#include <assert.h>
#include <stdint.h>

__attribute__((noinline))
void foo1 () {}

__attribute__((noinline))
__attribute__((optimize("align-functions=32")))
void foo2 () {}

int main ()
{
  foo1 ();
  foo2 ();

  void *f = &foo1;
  void *g = &foo2;

  assert (((uintptr_t)f % 16) == 0);
  assert (((uintptr_t)g % 32) == 0);
}
