extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_abs_fr1x16 (0x7777);
  if (t1 != 0x7777)
    abort ();

  return 0;
}

