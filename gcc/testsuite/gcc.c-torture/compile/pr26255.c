typedef short int16_t;

int round_sample(int *sum);

#define MULS(ra, rb) ((ra) * (rb))

#define SUM8(sum, op, w, p) \
{ \
  sum op MULS((w)[0 * 64], p[0 * 64]); \
  sum op MULS((w)[1 * 64], p[1 * 64]); \
  sum op MULS((w)[2 * 64], p[2 * 64]); \
  sum op MULS((w)[3 * 64], p[3 * 64]); \
  sum op MULS((w)[4 * 64], p[4 * 64]); \
  sum op MULS((w)[5 * 64], p[5 * 64]); \
  sum op MULS((w)[6 * 64], p[6 * 64]); \
  sum op MULS((w)[7 * 64], p[7 * 64]); \
}

void foo(int *dither_state, int *samples)
{
  int16_t *synth_buf;
  const int16_t *w, *p;
  int sum;

  sum = *dither_state;
  p = synth_buf + 16;
  SUM8(sum, +=, w, p);
  p = synth_buf + 48;
  SUM8(sum, -=, w + 32, p);
  *samples = round_sample(&sum);
}
