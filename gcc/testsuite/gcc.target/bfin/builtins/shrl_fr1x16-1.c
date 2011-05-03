extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_shrl_fr1x16 (0x8101, 4);
  if (t1 != 0x0810)
    abort ();

  return 0;
}

