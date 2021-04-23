/* PR target/71245 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentium -msse -mno-sse2 -mfpmath=387" } */

typedef union
{
  unsigned long long ll;
  double d;
} u_t;

u_t d = { .d = 5.0 };

void foo_d (void)
{
  u_t tmp;
  
  tmp.ll = __atomic_load_n (&d.ll, __ATOMIC_SEQ_CST);
  tmp.d += 1.0;
  __atomic_store_n (&d.ll, tmp.ll, __ATOMIC_SEQ_CST);
}

/* { dg-final { scan-assembler-not "movlps" { xfail *-*-* } } } */
