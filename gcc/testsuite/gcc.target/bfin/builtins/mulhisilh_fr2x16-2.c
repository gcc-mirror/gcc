extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short  fract16;

int main ()
{
  fract2x16 a, b;
  int t;

  a = __builtin_bfin_compose_2x16 (0x5000, 0x7000);
  b = __builtin_bfin_compose_2x16 (0xa000, 0x5000);

  t = __builtin_bfin_mulhisilh (a, b);
  if (t != 0xd6000000)
    abort ();
  return 0;
}

