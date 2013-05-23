/* PR middle-end/57344 */

struct __attribute__((packed)) S
{
  long long int a : 43;
  long long int b : 22;
  char c;
  long long int : 0;
} s[2];
int i;

__attribute__((noinline, noclone)) void
foo (long long int x)
{
  if (x != -3161LL)
    __builtin_abort ();
  asm volatile ("" : : : "memory");
}

int
main ()
{
  struct S t = { 0, -3161LL };
  s[1] = t;
  for (; i < 1; i++)
    foo (s[1].b);
  return 0;
}
