extern void abort (void);
extern void exit (int);

typedef int fract32;

int main ()
{
  fract32 f = 0x12345678, g;
  int a;

  a = __builtin_bfin_norm_fr1x32 (f);
  g = f << a;
  if (g != 0x48d159e0)
    abort ();

  exit (0);
}

