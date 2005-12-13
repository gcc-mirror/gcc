/* { dg-do compile { target { ilp32 } } } */
/* { dg-options "-O2" } */
struct Q 
{
  long x:20;
  long y:4;
  long z:8;
}b;
/* This should generate a single rl[w]imi. */
void rotins (unsigned int x)
{
  b.y = (x<<12) | (x>>20);
}

/* { dg-final { scan-assembler-not "inm" } } */
