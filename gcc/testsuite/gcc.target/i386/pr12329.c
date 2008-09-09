/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2" } */

extern void abort (void);

int test_nested (int i)
{
  int __attribute__ ((__noinline__, __regparm__(3))) foo(int j, int k, int l) /* { dg-error "nested functions are limited to 2 register parameters" } */
  {
    return i + j + k + l;
  }

  return foo (i, i+1, i+2);
}
