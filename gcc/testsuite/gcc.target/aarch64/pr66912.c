/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic" } */

__attribute__((visibility("protected")))
int n_common;

__attribute__((weak, visibility("protected")))
int n_weak_common;

__attribute__((visibility("protected")))
int n_init = -1;

__attribute__((weak, visibility("protected")))
int n_weak_init = -1;

int
f1 ()
{
  /* { dg-final { scan-assembler ":got(page_lo15)?:n_common" } } */
  return n_common;
}

int
f2 ()
{
  /* { dg-final { scan-assembler ":got(page_lo15)?:n_weak_common" } } */
  return n_weak_common;
}

int
f3 ()
{
  /* { dg-final { scan-assembler ":got(page_lo15)?:n_init" } } */
  return n_init;
}

int
f4 ()
{
  /* { dg-final { scan-assembler ":got(page_lo15)?:n_weak_init" } } */
  return n_weak_init;
}
