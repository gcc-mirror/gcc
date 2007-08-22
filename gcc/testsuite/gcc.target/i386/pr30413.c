/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int test() {
  char a, b = -1;
  asm volatile ("mov%z0 %1, %0" : "=q"(a) : "m"(b));
  return a;
}

int main()
{
  if (test() != -1)
    abort();

  return 0;
}
