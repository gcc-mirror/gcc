/* PR rtl-optimization/66152 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "movabs\[^\n\r]*506097522914230528" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movabs\[^\n\r]*505813836079825408" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "mov\[^\n\r]*50462976" { target ia32 } } } */
/* { dg-final { scan-assembler "mov\[^\n\r]*117835012" { target ia32 } } } */
/* { dg-final { scan-assembler "mov\[^\n\r]*100925952" { target ia32 } } } */
/* { dg-final { scan-assembler "mov\[^\n\r]*117768961" { target ia32 } } } */

void foo (char *);

void
bar (void)
{
  char a[] = {0,1,2,3,4,5,6,7};
  foo (a);
}

void
baz (void)
{
  char a[8] = "\0\2\4\6\1\3\5\7";
  foo (a);
}
