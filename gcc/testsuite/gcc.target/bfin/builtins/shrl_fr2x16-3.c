extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short fract16;

fract2x16 foo (fract2x16 f, short n)
{
  return __builtin_bfin_shrl_fr2x16 (f, n);
}

int main ()
{
  fract2x16 a, t;
  fract16 t1, t2;

  a = __builtin_bfin_compose_2x16 (0xcfff, 0xffff);

  t = foo (a, -4);
  t1 = __builtin_bfin_extract_hi (t);
  t2 = __builtin_bfin_extract_lo (t);
  if (t1 != 0xfffffff0 || t2 != 0xfffffff0)
    abort ();

  return 0;
}

