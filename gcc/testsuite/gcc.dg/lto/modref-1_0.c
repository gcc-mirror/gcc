/* { dg-lto-do run } */
/* { dg-lto-options {"-O2 -flto-partition=max -flto"}  } */
extern void recursive (int *a, int *b, int *c, int level);
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
