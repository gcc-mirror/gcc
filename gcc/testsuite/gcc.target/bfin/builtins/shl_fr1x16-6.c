extern void abort (void);

typedef short fract16;

fract16 foo (fract16 f, short n)
{
  return __builtin_bfin_shl_fr1x16 (f, n);
}

int main ()
{
  fract16 t1;

  t1 = foo (0x4004, -4);
  if (t1 != 0x0400)
    abort ();

  return 0;
}

