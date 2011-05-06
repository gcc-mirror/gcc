extern void abort (void);
extern void exit (int);

int main ()
{
  int a;

  a = __builtin_bfin_norm_fr1x32 (0x12345678);
  if (a != 2)
    abort ();

  exit (0);
}

