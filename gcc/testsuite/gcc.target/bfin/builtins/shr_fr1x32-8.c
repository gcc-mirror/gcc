extern void abort (void);

typedef long fract32;

fract32 foo (fract32 f, short n)
{
  return __builtin_bfin_shr_fr1x32 (f, n);
}

int main ()
{
  fract32 t;

  t = foo (0xc000e4ff, 4);
  if (t != 0xfc000e4f)
    abort ();

  return 0;
}

