/* { dg-do run } */
/* { dg-options "-Os -march=z13"  } */

#include <assert.h>
#include <stdint.h>

__attribute__((noinline))
void bar () {}

__attribute__((noinline))
__attribute__((optimize("O2")))
void baf () {}

int main ()
{
  bar ();
  baf ();

  void *g = &baf;

  assert ( ((uintptr_t)g % 16) == 0);
}
