extern void abort (void);

typedef short fract16;

fract16 foo (fract16 f, short n)
{
  return __builtin_bfin_shrl_fr1x16 (f, n);
}

int main ()
{
  fract16 t1;

  t1 = foo (0x8101, 4);
  if (t1 != 0x0810)
    abort ();

  return 0;
}

