extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_max_fr1x32 (0x80000000, 0xc0000000);
  if (t != 0xc0000000)
    abort ();

  return 0;
}

