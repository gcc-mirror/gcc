#define DO_REGREG_OPS(OP, NAME)				\
void varith_##NAME (__bf16 *dst, __bf16 *src, int count) \
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = dst[i] OP src[i];				\
}

#define DO_IMMEDIATE_OPS(VALUE, OP, NAME)		\
void varithimm_##NAME (__bf16 *dst, int count)		\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = dst[i] OP (__bf16) VALUE;			\
}

#define DO_ARITH_OPS(OP, NAME)				\
  DO_REGREG_OPS (OP, NAME);				\
  DO_IMMEDIATE_OPS (0.5, OP, NAME ## pointfive);	\
  DO_IMMEDIATE_OPS (2, OP, NAME ## 2);			\
  DO_IMMEDIATE_OPS (2.5, OP, NAME ## twopoint5);	\
  DO_IMMEDIATE_OPS (-0.5, OP, NAME ## minuspointfive);

DO_ARITH_OPS (+, add)
DO_ARITH_OPS (-, minus)
DO_ARITH_OPS (*, mult)
