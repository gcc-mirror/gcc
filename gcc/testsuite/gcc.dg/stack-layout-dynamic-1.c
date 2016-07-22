/* Verify that run time aligned local variables are aloocated in the prologue
   in one pass together with normal local variables.  */
/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

extern void bar (void *, void *, void *);
void foo (void)
{
  int i;
  __attribute__ ((aligned(65536))) char runtime_aligned_1[512];
  __attribute__ ((aligned(32768))) char runtime_aligned_2[1024];
  bar (&i, &runtime_aligned_1, &runtime_aligned_2);
}
/* { dg-final { scan-assembler-not "cfi_def_cfa_register" } } */
