extern void abort (void);

typedef long fract32;

fract32 foo (fract32 f, short n)
{
  return __builtin_bfin_shr_fr1x32 (f, n);
}

int main ()
{
  fract32 f;

  f = foo (0x87654321, 4);
  if (f != 0xf8765432)
    abort ();

  return 0;
}
