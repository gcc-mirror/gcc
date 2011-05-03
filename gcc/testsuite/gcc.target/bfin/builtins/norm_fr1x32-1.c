extern void abort (void);

int main ()
{
  int m;

  m = __builtin_bfin_norm_fr1x32 (0xefffeff1);
  if (m != 2)
    abort ();

  return 0;
}

