/* From PR rtl-optimization/19683.  On little-endian MIPS targets,
   reload would incorrectly inherit the high part of the multiplication
   result.  */
/* { dg-do run { target mips*-*-* } } */

extern void abort (void);
extern void exit (int);

#ifndef __mips16
#define REPEAT10(X, Y)					\
  X(Y##0); X(Y##1); X(Y##2); X(Y##3); X(Y##4);		\
  X(Y##5); X(Y##6); X(Y##7); X(Y##8); X(Y##9)

#define REPEAT30(X) REPEAT10 (X, 0); REPEAT10 (X, 1); REPEAT10 (X, 2)
#define IN(X) unsigned int x##X = ptr[0]
#define OUT(X) ptr[0] = x##X

union u { unsigned long long ll; unsigned int i[2]; };

unsigned int
foo (volatile unsigned int *ptr)
{
  union u u;
  int result;

  u.ll = (unsigned long long) ptr[0] * ptr[0];
  REPEAT30 (IN);
  REPEAT30 (OUT);
  asm ("#" : "=l" (result) : "l" (u.i[1]));
  return result;
}

int
main (void)
{
  unsigned int array[] = { 1000 * 1000 * 1000 };
  union u u;

  u.ll = (unsigned long long) array[0] * array[0];
  if (foo (array) != u.i[1])
    abort ();
  exit (0);
}
#else
int
main (void)
{
  exit (0);
}
#endif
