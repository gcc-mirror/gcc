extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_add_fr1x32 (0x40003000, 0xc000d000);
  if (t != 0x00010000)
    abort ();

  return 0;
}

