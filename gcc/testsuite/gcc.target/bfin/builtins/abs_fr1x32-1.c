extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_abs_fr1x32 (0x77777777);
  if (t != 0x77777777)
    abort ();

  return 0;
}

