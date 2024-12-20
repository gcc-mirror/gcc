/* { dg-do compile }  */
/* { dg-options "-std=c99" } */
/* { dg-additional-options "-O3 -march=armv8-a" } */

/*  Ensure correct creation of SVE Vector-length agnostic (VLA SVE) vector
    function calls from scalar versions in accordance with the Vector Function
    Application Binary Interface Specification for AArch64 (AAVPCS).

  We check for correctness in:
    - Vector function name mangling, with the grammar:

      vector name := prefix  "_" name
      prefix := "_ZGV" isa mask <len> <parameters>

      Whereby:
      - <isa>  := "s" for SVE
      - <mask> := "M" for Mask
      - <len>  := "x" for VLA SVE

      resulting in:
      <prefix> := "_ZGVsMx" <parameters>

      with each vector parameter contributing a "v" to the prefix.

    - Parameter and return value mapping:
      - Unless marked with uniform or linear OpenMP clauses, parameters and
	 return values are expected to map to vectors.
      - Where the lane-size of a parameter is less than the widest data size
	 for a given function, the resulting vector should be unpacked and
	 populated via extending loads.

    - Finally, we also make sure we can correctly generate calls to the same
      function, differing only in the target architecture (i.e. SVE vs SIMD),
      ensuring that each call points to the correctly-mangled vector function
      and employs the correct ABI.  For example, for `fn' we may expect:

	for #pragma GCC target("+sve"): _ZGVsMxvv_fn
	for #pragma GCC target("+simd): _ZGVnN4vv_fn */

#pragma GCC target ("+sve")
/* { dg-final { scan-assembler {\s+_ZGVsMxv_fn0\n} } } */
extern int __attribute__ ((simd, const)) fn0 (int);
void test_fn0 (int *a, int *b, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] += fn0 (b[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn1\n} } } */
extern int __attribute__ ((simd, const)) fn1 (short, int);
void test_fn1 (int *a, int *b, short *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = fn1 (c[i], b[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn2\n} } } */
extern short __attribute__ ((simd, const)) fn2 (short, int);
void test_fn2 (short *a, int *b, short *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = fn2 (c[i], b[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn3\n} } } */
extern char __attribute__ ((simd, const)) fn3 (int, char);
void test_fn3 (int *a, int *b, char *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = (int) (fn3 (b[i], c[i]) + c[i]);
}

/* { dg-final { scan-assembler {\s+_ZGVsMxvv_fn4\n} } } */
extern short __attribute__ ((simd, const)) fn4 (int, short);
void test_fn4 (int *a, int *b, short *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = (int) (fn4 (b[i], c[i]) + c[i]);
}

#pragma GCC reset_options
#pragma GCC target ("+simd")
/* { dg-final { scan-assembler {\s+_ZGVnN4vv_fn4\n} } } */
extern short __attribute__ ((simd, const)) fn4 (int, short);
void test_fn5 (int *a, int *b, short *c, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] = (int) (fn4 (b[i], c[i]) + c[i]);
}
