extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short  fract16;

int main ()
{
  fract2x16 a, b, t;
  fract16 t1, t2;

  a = __builtin_bfin_compose_2x16 (0x5000, 0x3000);
  b = __builtin_bfin_compose_2x16 (0x6000, 0x6000);

  t = __builtin_bfin_dspsubaddsat (a, b);
  t1 = __builtin_bfin_extract_hi (t);
  t2 = __builtin_bfin_extract_lo (t);
  if (t1 != -0x1000 || t2 != 0x7fff)
    abort ();
  return 0;
}

