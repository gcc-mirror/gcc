extern void abort (void);
extern void exit (int);

typedef int fract32;

int main ()
{
  fract32 f = 0xfedcba98, g;
  int a;

  a = __builtin_bfin_norm_fr1x32 (f);
  g = f << a;
  if (g != 0xb72ea600)
    abort ();

  exit (0);
}

