extern void abort (void);
extern void exit (int);

typedef short fract16;

int main ()
{
  fract16 f = 0x1234, g;
  int a;

  a = __builtin_bfin_norm_fr1x16 (f);
  g = f << a;
  if (g != (fract16) 0x48d0)
    abort ();

  exit (0);
}

