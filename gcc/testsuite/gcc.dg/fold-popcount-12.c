/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/112094 */
/* Test that
     (a != 0) | (popcount(a) == n) -> (a != 0) | (n == 0)
     (a == 0) & (popcount(a) != n) -> (a == 0) & (n != 0)
   and their commuted forms are folded away.  */

/* OR cases.  */
int f (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (a != 0) | (t == n);
}

int fl (unsigned long a, int n)
{
  int t = __builtin_popcountl (a);
  return (a != 0) | (t == n);
}

int fll (unsigned long long a, int n)
{
  int t = __builtin_popcountll (a);
  return (a != 0) | (t == n);
}

/* Signed-to-unsigned conversions inserted for popcount.  */
int f_signed (int a, int n)
{
  int t = __builtin_popcount (a);
  return (a != 0) | (t == n);
}

int fl_signed (long a, int n)
{
  int t = __builtin_popcountl (a);
  return (a != 0) | (t == n);
}

int fll_signed (long long a, int n)
{
  int t = __builtin_popcountll (a);
  return (a != 0) | (t == n);
}

/* Integral conversions: promotion, widening, and narrowing.  */
int f_promoted (short a, int n)
{
  int t = __builtin_popcount (a);
  return (a != 0) | (t == n);
}

int f_widened (int a, int n)
{
  int t = __builtin_popcountll (a);
  return (a != 0) | (t == n);
}

int f_narrowed (long long a, int n)
{
  int t = __builtin_popcount (a);
  return (a != 0) | (t == n);
}

/* Commuted operator and comparison operands.  */
int f_comm (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (t == n) | (a != 0);
}

int f_comm2 (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (a != 0) | (n == t);
}

int f_comm3 (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (0 != a) | (t == n);
}

/* AND cases.  */
int g (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (a == 0) & (t != n);
}

int gl (unsigned long a, int n)
{
  int t = __builtin_popcountl (a);
  return (a == 0) & (t != n);
}

int gll (unsigned long long a, int n)
{
  int t = __builtin_popcountll (a);
  return (a == 0) & (t != n);
}

/* Signed-to-unsigned conversions inserted for popcount.  */
int g_signed (int a, int n)
{
  int t = __builtin_popcount (a);
  return (a == 0) & (t != n);
}

/* Integral conversions: promotion, widening, and narrowing.  */
int g_promoted (short a, int n)
{
  int t = __builtin_popcount (a);
  return (a == 0) & (t != n);
}

int g_widened (int a, int n)
{
  int t = __builtin_popcountll (a);
  return (a == 0) & (t != n);
}

int g_narrowed (long long a, int n)
{
  int t = __builtin_popcount (a);
  return (a == 0) & (t != n);
}

/* Commuted operator and comparison operands.  */
int g_comm (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (t != n) & (a == 0);
}

int g_comm2 (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (a == 0) & (n != t);
}

int g_comm3 (unsigned int a, int n)
{
  int t = __builtin_popcount (a);
  return (0 == a) & (t != n);
}

/* { dg-final { scan-tree-dump-not "popcount" "optimized" } } */
