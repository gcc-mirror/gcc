/* { dg-do run } */

extern void abort (void);

int foo (int x)
{
  return (x / 2) / ((-__INT_MAX__ - 1) / -2);
}

int main()
{
  if (foo (- __INT_MAX__ - 1) != -1)
    abort ();
  return 0;
}
