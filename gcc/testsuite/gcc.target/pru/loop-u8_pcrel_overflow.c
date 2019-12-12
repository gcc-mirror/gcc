/* Test large loop bodies where U8_PCREL would overflow */

/* { dg-do run } */
/* { dg-options "-O1 -mloop" } */

/* -O1 in the options is significant.  Without it do-loop will not be
   run.  */

extern void abort (void);

#define OP	do { i1 <<= 2; i1 >>= 2; i2 *= 3; i2 /= 2; } while(0)
#define OP4	OP; OP; OP; OP
#define OP16	OP4; OP4; OP4; OP4
#define OP64	OP16; OP16; OP16; OP16
#define OP256	OP64; OP64; OP64; OP64

unsigned int
test_loop (unsigned int i1, unsigned i2)
{
  unsigned int i;
  volatile unsigned int s = 0;

  for (i = 0; i < 100; i++) {
    /* cannot use ASM NOP because it will prevent
       GCC from issuing a LOOP instruction. */
    OP256;
    s++;
  }
  return s + i1 + i2;
}

volatile unsigned int I1 = 0;
volatile unsigned int I2 = 0;

int
main (int argc, char** argv)
{
  if (test_loop (I1, I2) != 100)
    abort();

  return 0;
}
