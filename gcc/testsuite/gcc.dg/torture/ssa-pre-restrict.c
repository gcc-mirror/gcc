/* { dg-do run } */

extern void abort (void);

static inline int __attribute__((always_inline))
helper (int *__restrict__ a, int *b, int cond, int idx, int new_val)
{
  int v, v2;
  if (cond)
    {
      v = a[0];
      v2 = a[0];
    }
  else
    {
      v = b[0];
      b[idx] = new_val;
      v2 = b[0];
    }
  return v + v2;
}

int __attribute__((noipa))
foo (int *p, int cond, int idx, int new_val)
{
  int *q = p;
  return helper (p, q, cond, idx, new_val);
}

int
main (void)
{
  int val[2] = { 42, 0 };
  int res = foo (val, 0, 0, 100);
  if (res != 142)
    abort ();
  return 0;
}
