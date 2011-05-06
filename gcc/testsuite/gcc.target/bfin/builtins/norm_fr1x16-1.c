extern void abort (void);

typedef short fract16;

int main ()
{
  int m;

  m = __builtin_bfin_norm_fr1x16 (0x1000);
  if (m != 2)
    abort ();

  return 0;
}

