extern void abort (void);
extern void exit (int);

int main ()
{
  int a;

  a = __builtin_bfin_norm_fr1x32 (0xffffffff);
  if (a != 31)
    abort ();

  exit (0);
}

