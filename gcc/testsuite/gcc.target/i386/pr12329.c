/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2" } */

extern void abort (void);

int test_nested1 (int i)
{
  int __attribute__ ((__noinline__, __regparm__(3))) foo(int j, int k, int l)
  {
    return i + j + k + l;
  }

  return foo (i, i+1, i+2);
}

int test_nested2 (int i)
{
  int x;

  int __attribute__ ((__noinline__, __regparm__(3))) foo(int j, int k, int l)
  {
    return i + j + k + l;
  }

  x = foo (i+3, i+1, i+2);
  if (x != (4*i + 6))
    abort ();

  return x;
}

int
main ()
{
  int i = test_nested1 (3);

  if (i != 15)
    abort ();

  i = test_nested2 (4);

  if (i != 22)
    abort ();

  return 0;
}
