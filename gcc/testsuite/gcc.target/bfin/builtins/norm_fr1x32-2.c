extern void abort (void);

int main ()
{
  int m;

  m = __builtin_bfin_norm_fr1x32 (0x0000eff1);
  if (m != 15)
    abort ();

  return 0;
}

