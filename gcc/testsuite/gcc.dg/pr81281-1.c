/* PR sanitizer/81281 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "p_\[0-9]*\\(D\\)" "optimized" } } */

long long
f1 (int p, int a, int b)
{
  int c = p + 1;
  int d = a + 2;
  int e = b + 3;
  long long f = p + a;
  long long g = p + b;
  return f - g;
}

long long
f2 (int p, int a, int b)
{
  int c = a + 1;
  int d = p + 2;
  int e = b + 3;
  long long f = p + a;
  long long g = p + b;
  return f - g;
}

long long
f3 (int p, int a, int b)
{
  int c = b + 1;
  int d = p + 2;
  int e = a + 3;
  long long f = p + a;
  long long g = p + b;
  return f - g;
}

signed char
f4 (int p, int a, int b)
{
  int c = p + 1;
  int d = a + 2;
  int e = b + 3;
  signed char f = p + a;
  signed char g = p + b;
  return f - g;
}

signed char
f5 (int p, int a, int b)
{
  int c = a + 1;
  int d = p + 2;
  int e = b + 3;
  signed char f = p + a;
  signed char g = p + b;
  return f - g;
}

signed char
f6 (int p, int a, int b)
{
  int c = b + 1;
  int d = p + 2;
  int e = a + 3;
  signed char f = p + a;
  signed char g = p + b;
  return f - g;
}

long long
f7 (int p, int a)
{
  int c = p + 1;
  int d = a + 2;
  long long f = p + a;
  long long g = p;
  return f - g;
}

long long
f8 (int p, int a)
{
  int c = a + 1;
  int d = p + 2;
  long long f = p + a;
  long long g = p;
  return f - g;
}

signed char
f9 (int p, int a)
{
  int c = p + 1;
  int d = a + 2;
  signed char f = p + a;
  signed char g = p;
  return f - g;
}

signed char
f10 (int p, int a)
{
  int c = a + 1;
  int d = p + 2;
  signed char f = p + a;
  signed char g = p;
  return f - g;
}

long long
f11 (int p, int a)
{
  int c = p + 1;
  int d = a + 2;
  long long f = p;
  long long g = p + a;
  return f - g;
}

long long
f12 (int p, int a)
{
  int c = a + 1;
  int d = p + 2;
  long long f = p;
  long long g = p + a;
  return f - g;
}

signed char
f13 (int p, int a)
{
  int c = p + 1;
  int d = a + 2;
  signed char f = p;
  signed char g = p + a;
  return f - g;
}

signed char
f14 (int p, int a)
{
  int c = a + 1;
  int d = p + 2;
  signed char f = p;
  signed char g = p + a;
  return f - g;
}
