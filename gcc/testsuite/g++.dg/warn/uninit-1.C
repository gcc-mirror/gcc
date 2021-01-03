/* { dg-do compile } */
/* { dg-options "-O2 -Wmaybe-uninitialized" } */
struct a {int a;};
__attribute__ ((noinline))
void
nowarn (const struct a *ptr)
{
  if (ptr)
    asm volatile ("");
}
void
test()
{
  struct a ptr;
  nowarn (&ptr);
}
__attribute__ ((noinline))
int
nowarn2 (const struct a *ptr, const struct a ptr2)
{
  return ptr != 0 || ptr2.a;
}
int mem;
int
test2()
{
  struct a ptr,ptr2={0};
  return nowarn2 (&ptr, ptr2);
}
