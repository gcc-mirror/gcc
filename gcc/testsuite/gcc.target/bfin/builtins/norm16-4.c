extern void abort (void);
extern void exit (int);

typedef short fract16;

int main ()
{
  fract16 f = 0xfedc, g;
  int a;

  a = __builtin_bfin_norm_fr1x16 (f);
  g = f << a;
  if (g != (fract16) 0xb700)
    abort ();

  exit (0);
}

