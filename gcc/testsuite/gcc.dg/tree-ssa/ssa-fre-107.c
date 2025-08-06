/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

struct vec_char_16
{
  unsigned char raw[2];
};

static inline struct vec_char_16
Dup128VecFromValues(unsigned char t0, unsigned char t1)
{
  struct vec_char_16 result;
  result.raw[0] = t0;
  result.raw[1] = t1;
  return result;
}

int f(unsigned char t0, unsigned char t1)
{
  struct vec_char_16 a = Dup128VecFromValues(t0, t1);
  struct vec_char_16 b;
  __builtin_memcpy(&b, &a, sizeof(a));
  return b.raw[0] + b.raw[1];
}

/* Ideally we'd optimize this at FRE1 time but we only replace
   the loads from b.raw[] with BIT_FIELD_REFs which get optimized
   only later in the next FRE.  */
/* { dg-final { scan-tree-dump-not "MEM" "optimized" } } */
