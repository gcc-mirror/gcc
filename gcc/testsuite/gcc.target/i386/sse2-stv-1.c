/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mno-stackrealign" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

unsigned long long a,b,c,d;

static unsigned long rot(unsigned long long x, int y)
{
  /* Only called with y in 1..63.  */
  return (x<<y) | (x>>(64-y));
}

void foo()
{
    d = rot(d ^ a,32);
    c = c + d;
    b = rot(b ^ c,24);
    a = a + b;
    d = rot(d ^ a,16);
    c = c + d;
    b = rot(b ^ c,63);
}

/* { dg-final { scan-assembler-not "shldl" } } */
/* { dg-final { scan-assembler-not "%\[er\]sp" } } */
