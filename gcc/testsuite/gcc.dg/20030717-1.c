/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int zero (void)
{
  return 0;
}

int one (void)
{
  return 1;
}

int main (void)
{
  int i = 1;
  int r = (i ? one : zero)();
  if (r != 1)
    abort();
  return 0;
}

