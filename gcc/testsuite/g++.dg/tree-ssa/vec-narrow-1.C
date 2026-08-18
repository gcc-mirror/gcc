// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized -Wno-psabi" }
// { dg-additional-options "-mavx512vl -mavx512dq" { target { i?86-*-* x86_64-*-* } } }
// { dg-additional-options "-mmmx" { target ia32 } }
// Extension is monotone, so it commutes with the comparison and the outer
// truncation is exact.  The argument is lanewise, so a widened vector
// MIN/MAX feeding a truncating conversion narrows.
typedef int  v2si __attribute__((vector_size (8)));
typedef long long v2di __attribute__((vector_size (16)));
v2si f (v2si a, v2si b)
{ v2di x = __builtin_convertvector (a, v2di), y = __builtin_convertvector (b, v2di);
  return __builtin_convertvector (x < y ? x : y, v2si); }
// { dg-final { scan-tree-dump-not "vector\\(2\\) long" "optimized" } }
