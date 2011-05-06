extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_mult_fr1x32x32 (0x80000000, 0x80000000);
  if (t != 0x7fffffff)
    abort ();

  return 0;
}

