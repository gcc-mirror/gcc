/* PR tree-optimization/94800 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \* 72340172838076673" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \* 16843009" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \* 289360691352306692" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \* 1229782938247303441" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "<<" "optimized" } } */

unsigned long long int
foo (unsigned long long int x)
{
  unsigned long long int a = x + (x << 8);
  unsigned long long int b = a + (a << 16);
  unsigned long long int c = b + (b << 32);
  return c;
}

unsigned int
bar (unsigned int x)
{
  unsigned int a = x + (x << 8);
  unsigned int b = a + (a << 16);
  return b;
}

unsigned long long int
baz (unsigned long long int x)
{
  unsigned long long int a = (x << 2) + (x << 10);
  unsigned long long int b = a + (a << 16);
  unsigned long long int c = b + (b << 32);
  return c;
}

unsigned long long int
qux (unsigned long long int x)
{
  unsigned long long int a = x + (x << 4);
  unsigned long long int b = a + (a << 8);
  unsigned long long int c = b + (b << 16);
  unsigned long long int d = c + (c << 32);
  return d;
}

long long int
quux (long long int x)
{
  long long int a = x + (x << 8);
  long long int b = a + (a << 16);
  long long int c = b + (b << 32);
  return c;
}

int
corge (int x)
{
  int a = x + (x << 8);
  int b = a + (a << 16);
  return b;
}

long long int
garply (long long int x)
{
  long long int a = (x << 2) + (x << 10);
  long long int b = a + (a << 16);
  long long int c = b + (b << 32);
  return c;
}

long long int
waldo (long long int x)
{
  long long int a = x + (x << 4);
  long long int b = a + (a << 8);
  long long int c = b + (b << 16);
  long long int d = c + (c << 32);
  return d;
}
