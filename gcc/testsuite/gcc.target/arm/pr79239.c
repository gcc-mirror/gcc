/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok }  */
/* { dg-add-options arm_fp } */

#pragma GCC push_options
#pragma GCC target "fpu=crypto-neon-fp-armv8"
int a, b;
extern __inline __attribute__((__gnu_inline__)) int fn1() {}

#pragma GCC pop_options
void
fn2() {
  if (b * 0.77 + 0.5)
    a = 0;
}
