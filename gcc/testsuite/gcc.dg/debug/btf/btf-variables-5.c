/* Test BTF generation for extern variable with both non-defining and
   defining declarations.

   In this case, only a single variable record should be emitted,
   with 'global' linkage. However two array types will be generated.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect 1 variable with global (1) linkage.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1\[\t \]+\[^\n\]*btv_linkage" 1 } } */

/* Expect 2 array types, one of which is unsized.  For BPF target, -gprune-btf
   is the default and will remove the unsized array type.  */
/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*bta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*bta_nelems" 1 { target { !bpf-*-* } } } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*bta_nelems" 0 { target { bpf-*-* } } } } */

extern const char FOO[];
const char FOO[] = "foo";
