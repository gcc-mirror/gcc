/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

#define ARRAY(X) X##_array
#define DECLARE(X) \
  __thread int X; \
  __thread int ARRAY(X)[4]; \
  int *volatile *__attribute__((noinline)) \
  check##X (int *volatile *y) \
  { \
    if (!y || *y++ != &X || *y++ != &ARRAY(X)[3]) \
      return 0; \
    return y; \
  }
#define COPY(X) *y++ = &X; *y++ = &ARRAY(X)[3];
#define CHECK(X) y = check##X (y);
#define A(M, X) M(X##0) M(X##1) M(X##2) M(X##3) M(X##4) M(X##5) M(X##6) M(X##7)
#define B(M, X) A(M, X##0) A(M, X##1) A(M, X##2)
#define C(M, X) B(M, X) B(M, X) B(M, X)

#define NM 2
#define NA (NM * 8)
#define NB (NA * 3)
#define NC (NB * 3)

extern void abort (void);

B(DECLARE, tls)

void __attribute__ ((noinline))
setup (int *volatile *y)
{
  C(COPY, tls)
}

int
main (void)
{
  int *volatile array[NC];
  int *volatile *y = array;
  int i;

  setup (array);
  C(CHECK, tls);
  if (!y)
    abort ();
  return 0;
}
