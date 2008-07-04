/* { dg-do compile } */
int x;
volatile unsigned int y;

#define REPEAT10(X, Y)					\
  X(Y##0); X(Y##1); X(Y##2); X(Y##3); X(Y##4);		\
  X(Y##5); X(Y##6); X(Y##7); X(Y##8); X(Y##9)

#define REPEAT30(X) REPEAT10 (X, 0); REPEAT10 (X, 1); REPEAT10 (X, 2)
#define IN(X) unsigned int x##X = y
#define OUT(X) y = x##X

void __attribute__ ((interrupt_handler)) f1 (void)
{
  x = y + 11;
}

void __attribute__ ((interrupt_handler)) f2 (void)
{
  REPEAT30 (IN);
  REPEAT30 (OUT);
}
