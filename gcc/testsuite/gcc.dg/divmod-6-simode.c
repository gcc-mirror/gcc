/* { dg-require-effective-target divmod_simode } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */

typedef int SImode __attribute__((mode(SI)));
typedef unsigned USImode __attribute__((mode(SI)));

extern int cond;
void foo(void);

#define FOO(smalltype, bigtype, no)  \
bigtype f_##no(smalltype x, bigtype y) \
{					 \
  bigtype q = x / y;                     \
  bigtype r1 = 0, r2 = 0;                \
  if (cond)                              \
    r1 = x % y;                          \
  else                                   \
    r2 = x % y;                          \
  return q + r1 + r2;                    \
}

FOO(SImode, SImode, 1)
FOO(SImode, USImode, 2)
FOO(USImode, USImode, 3)

/* { dg-final { scan-tree-dump-times "DIVMOD" 3 "widening_mul" } } */
