/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);
extern void *malloc(__SIZE_TYPE__);

int * __attribute__((noinline,noclone))
foo (int *p)
{
  int *q = (int *) malloc (sizeof (int));
  *p = 1;
  *q = 2;
  if (*p != 1)
    __link_error ();
  *p = 3;
  return q;
}

int main()
{
  int i;
  int *p = foo (&i);
  if (i != 3 || *p != 2)
    abort ();
  return 0;
}
