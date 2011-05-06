extern void abort (void);
extern void exit (int);

int main ()
{
  int a;

  a = __builtin_bfin_norm_fr1x32 (0xfedcba98);
  if (a != 6)
    abort ();

  exit (0);
}

