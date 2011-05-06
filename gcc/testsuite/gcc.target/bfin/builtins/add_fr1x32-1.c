extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t;

  t = __builtin_bfin_add_fr1x32 (0x40003000, 0x50002000);
  if (t != 0x7fffffff)
    abort ();

  return 0;
}

