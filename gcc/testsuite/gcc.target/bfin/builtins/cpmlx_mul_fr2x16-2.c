extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short  fract16;

int main ()
{
  fract2x16 a, b, t;
  fract16 t1, t2;

  a = __builtin_bfin_compose_2x16 (0xa000, 0x8000);
  b = __builtin_bfin_compose_2x16 (0xb000, 0xe000);

  t = __builtin_bfin_cmplx_mul (a, b);
  t1 = __builtin_bfin_extract_hi (t);
  t2 = __builtin_bfin_extract_lo (t);
  if (t1 != 0x6800 || t2 != 0xffffe400)
    abort ();
  return 0;
}

