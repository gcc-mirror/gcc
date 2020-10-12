/* { dg-do run } */
/* { dg-options "-O2"  } */
short aa;
void
__attribute__ ((noinline, noclone))
recursive (int *a, int *b, int *c, int level)
{
  if (level && c)
    {
      recursive (b,a,c,0);
      aa++;
    }
  else
    *a=0;
}
int
main()
{
  int x = 123, y=124, z=125;
  recursive (&x,&y,&z,1);
  if (y)
    __builtin_abort ();
  if (!__builtin_constant_p (z))
    __builtin_abort ();
  return 0;
}
