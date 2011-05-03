extern void abort (void);

typedef long fract32;

fract32 foo (fract32 f, short n)
{
  return __builtin_bfin_shr_fr1x32 (f, n);
}

int main ()
{
  fract32 t;

  t = foo (0x7feff4ff, 4);
  if (t != 0x7feff4f)
    abort ();

  return 0;
}

