/* { dg-require-effective-target divmod } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */
/* mod comes before div in same bb.  */ 

typedef int SImode __attribute__((mode(SI)));
typedef unsigned USImode __attribute__((mode(SI)));

typedef int DImode __attribute__((mode(DI)));
typedef unsigned UDImode __attribute__((mode(DI)));

extern int cond;
void foo(void);

#define FOO(smalltype, bigtype, no)	\
bigtype f_##no(smalltype x, bigtype y)  \
{					\
  bigtype r = x % y;                    \
  bigtype q = x / y;                    \
  return q + r;                         \
}

FOO(SImode, DImode, 3)
FOO(SImode, UDImode, 4)
FOO(USImode, DImode, 6)
FOO(USImode, UDImode, 7)
FOO(DImode, DImode, 8)
FOO(DImode, UDImode, 9)
FOO(UDImode, UDImode, 10)

/* { dg-final { scan-tree-dump-times "DIVMOD" 7 "widening_mul" } } */
