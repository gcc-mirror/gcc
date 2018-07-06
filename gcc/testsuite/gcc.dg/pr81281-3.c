/* PR sanitizer/81281 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "\[+=-] \?123\[ ;]" "optimized" } } */

#ifdef __SIZEOF_INT128__
__int128
f1 (int a, long long b)
{
  __int128 f = 123 + a;
  __int128 g = 123 + b;
  return f - g;
}
#endif

signed char
f2 (int a, long long b)
{
  signed char f = 123 + a;
  signed char g = 123 + b;
  return f - g;
}

signed char
f3 (unsigned int a, unsigned long long b)
{
  signed char f = 123 + a;
  signed char g = 123 + b;
  return f - g;
}

unsigned char
f4 (unsigned int a, unsigned long long b)
{
  unsigned char f = 123 + a;
  unsigned char g = 123 + b;
  return f - g;
}

/* This isn't optimized yet.  */
#if 0
long long
f5 (int a)
{
  long long f = 123 + a;
  long long g = 123;
  return f - g;
}
#endif

signed char
f6 (long long a)
{
  signed char f = 123 + a;
  signed char g = 123;
  return f - g;
}

signed char
f7 (unsigned int a)
{
  signed char f = 123 + a;
  signed char g = 123;
  return f - g;
}

unsigned char
f8 (unsigned long int a)
{
  unsigned char f = 123 + a;
  unsigned char g = 123;
  return f - g;
}

long long
f9 (int a)
{
  long long f = 123;
  long long g = 123 + a;
  return f - g;
}

signed char
f10 (long long a)
{
  signed char f = 123;
  signed char g = 123 + a;
  return f - g;
}

signed char
f11 (unsigned int a)
{
  signed char f = 123;
  signed char g = 123 + a;
  return f - g;
}

unsigned char
f12 (unsigned long int a)
{
  unsigned char f = 123;
  unsigned char g = 123 + a;
  return f - g;
}
