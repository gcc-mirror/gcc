extern void abort (void);

typedef long fract32;

int main ()
{
  fract32 t1;

  t1 = __builtin_bfin_sub_fr1x32 (0x40003000, 0xc0003000);
  if (t1 != 0x7fffffff)
    abort ();

  return 0;
}

