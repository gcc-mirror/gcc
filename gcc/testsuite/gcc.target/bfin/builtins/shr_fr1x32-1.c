extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_shr_fr1x32 (0x7feff4ff, -4);
  if (t != 0x7fffffff)
    abort ();

  return 0;
}

