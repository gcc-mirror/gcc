/* { dg-do run { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
#include <locale.h>
#include <stdlib.h>
register int *testreg asm ("r29");

int x;
int y;
int *ext_func (int *p) { return p; }

void test_reg_save_restore (int*) __attribute__((noinline));
void
test_reg_save_restore (int *p)
{
    setlocale (LC_ALL, "C");
    testreg = ext_func(p);
}
int
main() {
  testreg = &x;
  test_reg_save_restore (&y);
  if (testreg != &y)
    abort ();
  return 0;
}
