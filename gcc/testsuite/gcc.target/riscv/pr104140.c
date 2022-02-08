/* { dg-do compile } */
/* { dg-options "-march=rv32im -mabi=ilp32" } */
int x;
unsigned u, v;
void f (void)
{
  long long y = x;
  u = y * v >> 32;
}
void g (void) { f (); }

