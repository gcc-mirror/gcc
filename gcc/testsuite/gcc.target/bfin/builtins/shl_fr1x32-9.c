extern void abort (void);

typedef long fract32;

fract32 foo (fract32 f, short n)
{
  return __builtin_bfin_shl_fr1x32 (f, n);
}

int main ()
{
  fract32 f;

  f = foo (0x12345678, 4);
  if (f != 0x7fffffff)
    abort ();

  return 0;
}
