extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_sub_fr1x32 (0x40003000, 0x70002000);
  if (t != 0xd0001000)
    abort ();

  return 0;
}

