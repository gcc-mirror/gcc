extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_mult_fr1x16 (0x0002, 0x0001);
  if (t1 != 0x0)
    abort ();

  return 0;
}

