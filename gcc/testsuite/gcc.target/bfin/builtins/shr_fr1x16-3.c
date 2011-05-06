extern void abort (void);

typedef short fract16;

fract16 foo (fract16 f, short n)
{
  return __builtin_bfin_shr_fr1x16 (f, n);
}

int main ()
{
  fract16 t1;

  t1 = foo (0x1101, -4);
  if (t1 != 0x7fff)
    abort ();

  return 0;
}

