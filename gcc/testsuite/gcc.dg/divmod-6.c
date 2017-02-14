/* { dg-require-effective-target divmod } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */

typedef int SImode __attribute__((mode(SI)));
typedef unsigned USImode __attribute__((mode(SI)));

typedef int DImode __attribute__((mode(DI)));
typedef unsigned UDImode __attribute__((mode(DI)));

extern int cond;
void foo(void);

#define FOO(smalltype, bigtype, no)	 \
bigtype f_##no(smalltype x, bigtype y)   \
{					 \
  bigtype q = x / y;                     \
  bigtype r1 = 0, r2 = 0;                \
  if (cond)                              \
    r1 = x % y;                          \
  else                                   \
    r2 = x % y;                          \
  return q + r1 + r2;                    \
}

FOO(SImode, DImode, 3)
FOO(SImode, UDImode, 4)
FOO(USImode, DImode, 6)
FOO(USImode, UDImode, 7)
FOO(DImode, DImode, 8)
FOO(DImode, UDImode, 9)
FOO(UDImode, UDImode, 10)

/* { dg-final { scan-tree-dump-times "DIVMOD" 7 "widening_mul" } } */
