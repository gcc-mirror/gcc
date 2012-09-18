/* The dynamic shift library functions truncate the shift count to 5 bits.
   Verify that this is taken into account and no extra shift count
   truncations are generated before the library call.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m1*" "-m2" "-m2e*" } } */
/* { dg-final { scan-assembler-not "and" } } */
/* { dg-final { scan-assembler-not "31" } } */

int
test00 (unsigned int a, int* b, int c, int* d, unsigned int e)
{
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s += d[i] + b[i] + (e << (i & 31));
  return s;  
}

int
test01 (unsigned int a, int* b, int c, int* d, unsigned int e)
{
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s += d[i] + b[i] + (e >> (i & 31));
  return s;  
}

int
test03 (unsigned int a, unsigned int b)
{
  return b << (a & 31);
}

unsigned int
test04 (unsigned int a, int b)
{
  return a >> (b & 31);
}
