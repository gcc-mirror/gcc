/* Alias to alias; 'libgomp.c-c++-common/pr96390.c'.  */

/* { dg-do compile } */
/* { dg-add-options ptx_alias } */

int v;

void foo () { v = 42; }
void bar () __attribute__((alias ("foo")));
void baz () __attribute__((alias ("bar")));

int
main (void)
{
  baz ();
  if (v != 42)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "\\.alias bar,foo;" 1 } } */
/* { dg-final { scan-assembler-times "\\.visible \\.func foo;" 1 } } */
/* { dg-final { scan-assembler-times "\\.visible \\.func bar;" 1 } } */

/* { dg-final { scan-assembler-times "\\.alias baz,bar;" 1 } } */
/* { dg-final { scan-assembler-times "\\.visible \\.func baz;" 1 } } */
