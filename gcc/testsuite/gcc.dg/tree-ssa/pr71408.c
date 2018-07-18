/* PR middle-end/71408 */
/* { dg-do run } */
/* { dg-options "-Os" } */

#if __SIZEOF_INT__ >= 4
unsigned a, b;

struct S0
{
  int f1:18;
  unsigned f3:4;
};
#else
__UINT32_TYPE__ a, b;

struct S0
{
  __INT32_TYPE__ f1:18;
  unsigned f3:4;
};
#endif

void fn1 ()
{
  struct S0 c = { 7, 0 };
  if (c.f1)
    c.f3 = 3;
  a = -~c.f3;
  c.f3 = ~(c.f1 && c.f1);
  c.f1 = c.f3 * (c.f1 - (c.f1 - a % c.f1)) + ~c.f3 * -a;
  b = ~(c.f1 & a);
  if (b >= 4294967295)
    __builtin_abort ();
}

int
main ()
{
  fn1 ();
  return 0;
}
