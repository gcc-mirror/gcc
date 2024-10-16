/* { dg-additional-options "-std=gnu17" } */
/* { dg-skip-if "PR112705" { hppa*64*-*-* } } */
int a, b;
void d();
void c()
{
  d((void (*)()) & a + b);
}
