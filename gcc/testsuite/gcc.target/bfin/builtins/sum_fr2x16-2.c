extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));
typedef __v2hi fract2x16;
typedef short fract16;

int main ()
{
  fract2x16 a;
  fract16 t;

  a = __builtin_bfin_compose_2x16 (0x1001, 0x0001);

  t = __builtin_bfin_sum_fr2x16 (a);
  if (t != 0x1002)
    abort ();

  return 0;
}

