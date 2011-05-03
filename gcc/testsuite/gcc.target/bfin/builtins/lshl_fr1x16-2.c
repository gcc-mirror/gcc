extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_lshl_fr1x16 (0x4004, -4);
  if (t1 != 0x0400)
    abort ();

  return 0;
}

