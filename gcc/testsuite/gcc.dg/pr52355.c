/* { dg-do compile } */

void f(char a[16][16][16])
{
  __asm volatile ("" : : "i" (&a[1][0][0] - &a[0][0][0]));
}

int main(void)
{
  char a[16][16][16];
  f(a);
  return 0;
}
