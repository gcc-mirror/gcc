/* { dg-require-effective-target divmod_simode } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */
/* mod comes before div in same bb.  */ 

typedef int SImode __attribute__((mode(SI)));
typedef unsigned USImode __attribute__((mode(SI)));

extern int cond;
void foo(void);

#define FOO(smalltype, bigtype, no)      \
bigtype f_##no(smalltype x, bigtype y)   \
{					 \
  bigtype r = x % y;                     \
  bigtype q = x / y;                     \
  return q + r;                          \
}

FOO(SImode, SImode, 1)
FOO(SImode, USImode, 2)
FOO(USImode, USImode, 3)

/* { dg-final { scan-tree-dump-times "DIVMOD" 3 "widening_mul" } } */
