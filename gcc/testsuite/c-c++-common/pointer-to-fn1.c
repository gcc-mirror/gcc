/* PR c++/106937 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fpermissive -fcf-protection" } */
/* { dg-additional-options "-std=c++11" { target c++ } } */
/* Test printing a pointer to function with attribute.  */

__attribute__((nocf_check)) typedef void (*FPA1)();
[[gnu::nocf_check]] typedef void (*FPA2)(int);
typedef void (*FP1)();
typedef void (*FP2)(int);

void
g (FP1 f1, FP2 f2)
{
  FPA1 p1 = f1; // { dg-warning {aka 'void \(__attribute__\(\(nocf_check\)\) \*\)\(\)'} }
  FPA2 p2 = f2; // { dg-warning {aka 'void \(\*\)\(int\) \[\[gnu::nocf_check\]\]'} }
  FP1 p3 = p1; // { dg-warning {aka 'void \(__attribute__\(\(nocf_check\)\) \*\)\(\)'} }
  FP2 p4 = p2; // { dg-warning {aka 'void \(\*\)\(int\) \[\[gnu::nocf_check\]\]'} }
}
