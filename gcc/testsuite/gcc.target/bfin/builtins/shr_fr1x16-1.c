extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_shr_fr1x16 (0x1101, -4);
  if (t1 != 0x7fff)
    abort ();

  return 0;
}

