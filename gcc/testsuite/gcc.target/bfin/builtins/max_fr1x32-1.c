extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_max_fr1x32 (0x77777777, 0x70007000);
  if (t != 0x77777777)
    abort ();

  return 0;
}

