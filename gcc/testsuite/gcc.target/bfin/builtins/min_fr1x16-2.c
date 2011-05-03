extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_min_fr1x16 (0x7000, 0xc001);
  if (t1 != -0x3fff) 
    abort ();

  return 0;
}

