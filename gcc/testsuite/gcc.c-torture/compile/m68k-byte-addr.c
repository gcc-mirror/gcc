/* This testcase triggered an attempt to reload a byte value into an
   address register.  */
extern volatile unsigned char x[];

#define DECLARE(I) orig##I, inc##I
#define READ(I) orig##I = x[I]
#define INC(I) inc##I = orig##I + 1
#define WRITE1(I) x[I] = orig##I
#define WRITE2(I) x[I] = inc##I

#define REPEAT(X) X(0), X(1), X(2), X(3), X(4), X(5), X(6), X(7), X(8)

void foo (void)
{
  unsigned char REPEAT (DECLARE);
  REPEAT (READ);
  REPEAT (INC);
  REPEAT (WRITE1);
  REPEAT (WRITE2);
}
