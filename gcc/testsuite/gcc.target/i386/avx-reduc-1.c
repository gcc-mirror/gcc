/* { dg-do run } */
/* { dg-options "-O3 -mavx" } */
/* { dg-require-effective-target avx_runtime } */

extern void abort (void);
double ad[1024];
float af[1024];
short as[1024];
int ai[1024];
long long all[1024];
unsigned short aus[1024];
unsigned int au[1024];
unsigned long long aull[1024];

#define F(var) \
__attribute__((noinline, noclone)) __typeof (var[0]) \
f##var (void) \
{ \
  int i; \
  __typeof (var[0]) r = 0; \
  for (i = 0; i < 1024; i++) \
    r = r > var[i] ? r : var[i]; \
  return r; \
}

#define TESTS \
F (ad) F (af) F (as) F (ai) F (all) F (aus) F (au) F (aull)

TESTS

int
main ()
{
  int i;
  for (i = 0; i < 1024; i++)
    {
#undef F
#define F(var) var[i] = i;
      TESTS
    }
  for (i = 1023; i < 32 * 1024; i += 1024 + 271)
    {
#undef F
#define F(var) var[i & 1023] = i; if (f##var () != i) abort ();
      TESTS
    }
  return 0;
}
