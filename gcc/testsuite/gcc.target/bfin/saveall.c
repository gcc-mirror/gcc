/* { dg-do run { target bfin*-*-* } } */
/* { dg-options "-fomit-frame-pointer" } */

void foo (void) __attribute__ ((saveall));
void foo ()
{
  asm ("R0 = 0; RETS = R0;");
}

int main ()
{
  foo ();
  return 0;
}
