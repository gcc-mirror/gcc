/* PR optimization/12419 */
/* Ensure external_const_array[0] is read just once.  */
/* { dg-do compile { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "external_const_array" } } */
/* { dg-final { scan-assembler-not "external_const_array.*add\[^\\n\]*external_const_array" } } */

extern const int external_const_array [];
extern void foo (void);

int
bar (void)
{
  int n = external_const_array[0];
  foo ();
  n += external_const_array[0];
  return n;
}
