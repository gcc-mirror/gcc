extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_shl_fr1x32 (0xc000e4ff, -4);
  if (t != 0xfc000e4f)
    abort ();

  return 0;
}

