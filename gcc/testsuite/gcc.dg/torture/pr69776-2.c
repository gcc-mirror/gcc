/* { dg-do run } */
/* { dg-additional-options "-fstrict-aliasing" } */

extern void *malloc (__SIZE_TYPE__);
extern void abort (void);

__attribute__((noinline,noclone))
void f(int *qi, double *qd)
{
  int i = *qi;
  *qd = 0;
  *qi = i;
}

int main()
{
  int *p = malloc(sizeof(double));

  *p = 1;
  f(p, (double *)p);
  if (*p != 1)
    abort();
  return 0;
}

