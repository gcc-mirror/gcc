extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_mult_fr1x32 (0x7777, 0x0001);
  if (t != 0x0000eeee)
    abort ();

  return 0;
}

