/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm32 } */

#define min(x, y) ((x) <= (y)) ? (x) : (y)

unsigned int 
foo (unsigned int i, unsigned int x, unsigned int y)
{
  return i < (min (x, y));
}

int 
bar (int i, int x, int y)
{
  return i < (min (x, y));
}

/* { dg-final { scan-assembler "cmpcs" } } */
/* { dg-final { scan-assembler "cmpge" } } */
