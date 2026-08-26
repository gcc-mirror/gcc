/* { dg-do compile { target { x86 || aarch64*-*-* } } } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -Wno-psabi" } */
/* { dg-additional-options "-msse2" { target x86 } } */
/* Combine pairs of vector comparisons against zero or all-ones.  Use
   different signedness for the operands to exercise the view conversion.  */
typedef int v4si __attribute__((vector_size (16)));
typedef unsigned int v4ui __attribute__((vector_size (16)));

v4si f1 (v4si a, v4ui b) { return (a == 0) & (b == 0); }
v4si f2 (v4si a, v4ui b) { return (a != 0) | (b != 0); }
v4si f3 (v4si a, v4ui b) { return (a == -1) & (b == -1u); }
v4si f4 (v4si a, v4ui b) { return (a != -1) | (b != -1u); }

/* Each function keeps one bitwise operation and one comparison before
   target-specific vector lowering.  */
/* { dg-final { scan-tree-dump-times " == " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " != " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " \\| " 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " & " 2 "forwprop1" } } */
