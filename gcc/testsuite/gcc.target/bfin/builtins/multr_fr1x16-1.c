extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_multr_fr1x16 (0x7777, 0x0007);
  if (t1 != 0x0007)
    abort ();

  return 0;
}

