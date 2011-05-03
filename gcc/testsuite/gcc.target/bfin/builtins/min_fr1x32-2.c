extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_min_fr1x32 (0x70007000, 0xc000c000);
  if (t != 0xc000c000)
    abort ();

  return 0;
}

