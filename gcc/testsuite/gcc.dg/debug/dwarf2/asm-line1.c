/* PR debug/50983 */
/* { dg-do compile { target *-*-linux-gnu } } */
/* { dg-options "-O0 -gdwarf-2" } */
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
