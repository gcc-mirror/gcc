// { dg-do compile }
// { dg-options "-O2 -fdump-tree-forwprop1" }
// { dg-additional-options "-msse4" { target x86 } }
// { dg-require-effective-target int128 { target x86 } }

typedef int v2si __attribute__((vector_size (8)));
typedef long long v2di __attribute__((vector_size (16)));

v2si
f (v2si a, v2si b, v2di *p)
{
  v2di x = __builtin_convertvector (a, v2di);
  v2di y = __builtin_convertvector (b, v2di);
  v2di z = x < y ? x : y;
  *p = z;
  return __builtin_convertvector (z, v2si);
}

// A shared wide MIN must not gain a second narrow MIN.
// { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "forwprop1" } }
