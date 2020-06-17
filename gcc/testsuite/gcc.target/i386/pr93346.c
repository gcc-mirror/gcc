/* PR target/93346 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi2" } */
/* { dg-final { scan-assembler-times "\tbzhi\t" 12 } } */

unsigned int
f1 (unsigned int x, unsigned int y)
{
  return x & ((1 << y) - 1);
} 

unsigned int
f2 (unsigned int x, unsigned int y)
{
  return x & ((1U << y) - 1);
} 

int
f3 (int x, unsigned int y)
{
  return x & ((1 << y) - 1);
} 

unsigned long
f4 (unsigned long x, unsigned int y)
{
  return x & ((1L << y) - 1);
} 

unsigned long
f5 (unsigned long x, unsigned int y)
{
  return x & ((1UL << y) - 1);
} 

long
f6 (long x, unsigned int y)
{
  return x & ((1L << y) - 1);
} 

unsigned int
f7 (unsigned int x, int y)
{
  return x & ((1 << y) - 1);
} 

unsigned int
f8 (unsigned int x, int y)
{
  return x & ((1U << y) - 1);
} 

int
f9 (int x, int y)
{
  return x & ((1 << y) - 1);
} 

unsigned long
f10 (unsigned long x, int y)
{
  return x & ((1L << y) - 1);
} 

unsigned long
f11 (unsigned long x, int y)
{
  return x & ((1UL << y) - 1);
} 

long
f12 (long x, int y)
{
  return x & ((1L << y) - 1);
} 
