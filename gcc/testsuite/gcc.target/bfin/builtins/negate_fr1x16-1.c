extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_negate_fr1x16 (0x7fff);
  if (t1 != -0x7fff) 
    abort ();

  return 0;
}

