/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mcmodel=large -mlarge-data-threshold=4" } */
/* { dg-skip-if "PR90698" "*-*-darwin*" } */
/* { dg-final { scan-assembler {\.lbss} } } */
/* { dg-final { scan-assembler {\.bss} } } */
/* { dg-final { scan-assembler {\.ldata} } } */
/* { dg-final { scan-assembler {\.data} } } */
/* { dg-final { scan-assembler {\.lrodata} } } */
/* { dg-final { scan-assembler {\.rodata} } } */

const char rodata_a[] = "abc", rodata_b[] = "abcd";
char data_a[4] = {1}, data_b[5] = {1};
char bss_a[4], bss_b[5];
