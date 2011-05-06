extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_mult_fr1x32x32 (0x7fff0000, 0x00000007);
  if (t != 0x6)
    abort ();

  return 0;
}

