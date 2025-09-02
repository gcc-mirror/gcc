/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7_ok } */
/* { dg-options "-O2 -fstack-clash-protection -fnon-call-exceptions" } */
/* { dg-add-options arm_arch_v7 } */
/* { dg-final { scan-assembler-not {#-8} } } */
/* LRA register elimination gets confused when register spilling
   causes arm_frame_pointer_required to switch from false to true, and
   ends up using a stack slot below sp.  */

void f() {
  int i = 0, j = 0;
  asm ("" : : "m" (i), "m" (j));
}

void g(void (*fn[])(), int i)
{
  auto fn0 = fn[i+0];
  auto fn1 = fn[i+1];
  auto fn2 = fn[i+2];
  auto fn3 = fn[i+3];
  fn0();
  fn1();
  if (!fn2)
    throw i+2;
  fn2();
  fn3();
  fn0();
  fn1();
}

int
main()
{
  void (*fn[4])() = { f, f, f, f };
  g (fn, 0);
}
