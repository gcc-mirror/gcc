// { dg-do compile }
// For slim LTO there's no optimized dump
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-additional-options "-fdump-tree-optimized" }

typedef double T;
static int equalfn (volatile T* x, volatile T* y);
T gx, gy;
int main ()
{
  T x = gx, y = gy;
  return equalfn (&x, &y);
}
static int equalfn (volatile T* x, volatile T* y)
{
  return (*x == *y);
}

// There should be exactly two volatile accesses (ignoring clobbers).
// { dg-final { scan-tree-dump-times " ={v} \[^\{\]" 2 "optimized" } }
