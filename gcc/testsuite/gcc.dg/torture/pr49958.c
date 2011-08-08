/* { dg-do run } */
/* { dg-options "-fstrict-overflow" } */

extern void abort (void);
int foo (int i, int j, int o, int m) { return i*o + 1 + j*m > 1; }
int main()
{
  if (foo (- __INT_MAX__ - 1, -1, 1, 1))
    abort ();
  return 0;
}
