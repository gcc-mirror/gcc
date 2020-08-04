/* { dg-do run } */
/* { dg-options "-O2 -mno-sse" } */

int flag;
union { double f; unsigned long long i; } u;
void __attribute__((noinline))
init ()
{
  flag = 1;
  u.i = 18442936822990639076ULL;
}
unsigned long long __attribute__((noinline))
test ()
{
  if (flag)
    return u.i;
  else
    return u.f;
}
int main()
{
  init ();
  if (test () != 18442936822990639076ULL)
    __builtin_abort ();
  return 0;
}
