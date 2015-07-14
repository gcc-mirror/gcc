/* { dg-do run } */
/* { dg-options "-O2 -mregparm=3" } */
/* { dg-require-effective-target ia32 } */
extern void abort (void);

int s (int i, int j, int k, int l)
{
  __label__ l1;
    int f (int i, int j, int k, int l)
    {
      if (i + j + k + l == 10)
	goto l1;
      return 0;
    }
    return f (i, j, k, l);
 l1:;
    return 1;
}

int main ()
{
  if (s (1, 2, 3, 4) != 1)
      abort ();

  return 0;
}
