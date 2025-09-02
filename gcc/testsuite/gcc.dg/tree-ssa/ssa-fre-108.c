/* { dg-do compile } */
/* { dg-options "-O -Wno-psabi -fdump-tree-fre1" } */

#define vector16 __attribute__((vector_size(16)))
#define vector32 __attribute__((vector_size(32)))

union u1
{
  struct s1
  {
    vector16 int hi;
    vector16 int low;
  }hilow;
  vector32 int v;
};

vector16 float f(vector16 int a, vector16 int b)
{
  union u1 c;
  c.hilow.hi = a;
  c.hilow.low = b;
  vector32 int d0 = c.v;
  vector32 float d = (vector32 float)d0;
  vector16 float e = __builtin_shufflevector (d, d, 0, 1, 2, 3);
  vector16 float f = __builtin_shufflevector (d, d, 4, 5, 6, 7);
  return e/f;
}

/* { dg-final { scan-tree-dump-times "_\[0-9\]\+ = VIEW_CONVERT_EXPR" 2 "fre1" } } */
/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "fre1" } } */
