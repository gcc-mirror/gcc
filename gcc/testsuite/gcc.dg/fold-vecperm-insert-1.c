/* { dg-do compile { target aarch64*-*-* arm*-*-* x86_64-*-* } } */
/* { dg-require-effective-target arm_neon_ok { target arm*-*-* } } */
/* { dg-options "-O2 -fdump-tree-forwprop3" } */
/* { dg-add-options arm_neon } */

typedef int __attribute__((vector_size(16))) v4si;

/* Shift vector v by one element and insert the value val.  The vector shift
   typically requires a zero vector operand in VEC_PERM_EXPR, but it can be
   optimized away in this case.  */

v4si shift_and_insert (v4si v, int val)
{
  v4si zero = { 0, 0, 0, 0 };
  v4si sel = { 1, 2, 3, 4 };
  v4si shifted = __builtin_shuffle (v, zero, sel);
  shifted[3] = val;
  return shifted;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR.*v_\[0-9\]+.*v_\[0-9\]+" 1 "forwprop3" } } */
