/* { dg-do run } */
/* { dg-options "-O2" } */

int __attribute__((noinline))
foo(char c)
{
  unsigned short a = (unsigned short)c;
  if (a >= -32000 && a <= 32000)
    return c == 0;
  return -1;
}

extern void abort (void);

int main()
{
  if (foo(1) == 1)
    abort ();
  return 0;
}

