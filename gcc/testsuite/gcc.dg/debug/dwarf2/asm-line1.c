/* PR debug/50983 */
/* { dg-do compile { target *-*-gnu* } } */
/* { dg-options "-O0 -gdwarf" } */
/* { dg-final { scan-assembler "is_stmt 1" } } */

int i;
void f() __attribute ((section ("foo")));
void f() { if (i) ++i; else --i; }

void fun()
{
  return;
}

int main()
{
  f();
  fun();
  return 0;
}
