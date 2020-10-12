/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-Og -fno-early-inlining -fno-tree-ccp -fno-tree-dce" } */
/* { dg-additional-options "-mavx -mno-sse4.2" { target x86_64-*-* i?86-*-* } } */

typedef int __attribute__ ((__vector_size__ (16))) U;
typedef unsigned long __attribute__ ((__vector_size__ (16))) V;

static inline int
bar (unsigned long e, V f)
{
  V g = f != e;
  (union {U b;}){(U) g};
}

void
foo (void)
{
  int j = bar (8, (V) { });
  for (unsigned i;; i[&j])
    ;
}
