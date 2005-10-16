/* PR c/11420 */
/* { dg-do link { target fpic } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-bogus "\[Uu\]nresolved symbol .(_GLOBAL_OFFSET_TABLE_|\[_.A-Za-z\]\[_.0-9A-Za-z\]*@(PLT|GOT|GOTOFF))" "PIC unsupported" { xfail *-*-netware* } 0 } */

void (* volatile fn) (void);
static void foo (void)
{
}

int main (void)
{
  fn = foo;
  return 0;
}
