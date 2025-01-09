/* PR tree-optimization/117927 */
/* { dg-do compile { target { int32 && longlong64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " r<< " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " r>> " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 31;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 63;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "32 - " "optimized" } } */
/* { dg-final { scan-tree-dump-not "64 - " "optimized" } } */

static inline
unsigned lrotate32 (unsigned x, int t)
{
  unsigned tl = x << t;
  unsigned th = x >> (-t & 31);
  return tl | th;
}

static inline
unsigned rrotate32 (unsigned x, int t)
{
  unsigned tl = x >> t;
  unsigned th = x << (-t & 31);
  return tl | th;
}

static inline
unsigned long long lrotate64 (unsigned long long x, int t)
{
  unsigned long long tl = x << t;
  unsigned long long th = x >> (-t & 63);
  return tl | th;
}

static inline
unsigned long long rrotate64 (unsigned long long x, int t)
{
  unsigned long long tl = x >> t;
  unsigned long long th = x << (-t & 63);
  return tl | th;
}

unsigned
f1 (unsigned x, int t)
{
  return lrotate32 (x, 32 - t);
}

unsigned long long
f2 (unsigned long long x, int t)
{
  return lrotate64 (x, 64 - t);
}

unsigned
f3 (unsigned x, int t)
{
  if (t == 32)
    __builtin_unreachable ();
  return lrotate32 (x, 32 - t);
}

unsigned long long
f4 (unsigned long long x, int t)
{
  if (t == 64)
    __builtin_unreachable ();
  return lrotate64 (x, 64 - t);
}

unsigned
f5 (unsigned x, int t)
{
  return rrotate32 (x, 32 - t);
}

unsigned long long
f6 (unsigned long long x, int t)
{
  return rrotate64 (x, 64 - t);
}

unsigned
f7 (unsigned x, int t)
{
  if (t == 32)
    __builtin_unreachable ();
  return rrotate32 (x, 32 - t);
}

unsigned long long
f8 (unsigned long long x, int t)
{
  if (t == 64)
    __builtin_unreachable ();
  return rrotate64 (x, 64 - t);
}
