/* { dg-lto-do run } */

int __atoi  (const char *) __asm__("atoi");
extern inline __attribute__((always_inline,gnu_inline))
int atoi (const char *x)
{
  return __atoi (x);
}

int bar (int (*)(const char *));

int main()
{
  return bar (atoi);
}
