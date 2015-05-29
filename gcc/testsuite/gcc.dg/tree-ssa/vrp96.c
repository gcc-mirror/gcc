/* PR tree-optimization/65170 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

typedef unsigned __int128 T;
extern void link_error (void);
extern void required_check (void);

T
foo (T b, T c)
{
  T a;
  b &= 0xffffffffffffffffULL;
  c &= 0xffffffffffffffffULL;
  if (b < 7 || c < 7)
    return 0;
  a = b * c;
  if (a < 49 || a > (((T) 0xfffffffffffffffeULL << 64) | 1))
    link_error ();
  return a;
}

T
bar (T b, T c)
{
  T a;
  b &= 0xffffffffffffffffULL;
  c &= 0xffffffffffffffffULL;
  if (b < 7 || c < 7)
    return 0;
  a = b * c;
  if (a == 49)
    required_check ();
  return a;
}

T
baz (T b, T c)
{
  T a;
  b &= 0xffffffffffffffffULL;
  c &= 0xffffffffffffffffULL;
  if (b < 7 || c < 7)
    return 0;
  a = b * c;
  if (a == (((T) 0xfffffffffffffffeULL << 64) | 1))
    required_check ();
  return a;
}

/* { dg-final { scan-tree-dump-not "link_error" "vrp1" } } */
/* { dg-final { scan-tree-dump-times "required_check" 2 "vrp1" } } */
