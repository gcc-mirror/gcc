/* GCSE used to reuse the value of __MRDACC.  */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

void foo (int *z)
{
  __MWTACC (3, 1);
  if (__MRDACC (3) != 1)
    *z = 1;
  __MCLRACCA ();
  if (__MRDACC (3) != 1)
    *z = 2;
}

int main ()
{
  int z = 3;

  foo (&z);
  if (z != 2)
    abort ();
  exit (0);
}
