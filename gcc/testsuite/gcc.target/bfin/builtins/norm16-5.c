extern void abort (void);
extern void exit (int);

int main ()
{
  int a;

  a = __builtin_bfin_norm_fr1x16 (0);
  if (a != 15)
    abort ();

  exit (0);
}

