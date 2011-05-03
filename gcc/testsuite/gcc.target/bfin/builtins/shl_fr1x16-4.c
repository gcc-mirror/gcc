extern void abort (void);

typedef short fract16;

int main ()
{
  fract16 t1;

  t1 = __builtin_bfin_shl_fr1x16 (0xd004, -4);
  if (t1 != 0xfffffd00)
    abort ();

  return 0;
}

