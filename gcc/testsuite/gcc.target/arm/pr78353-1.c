/* { dg-do link }  */
/* { dg-require-effective-target arm_arch_v7a_multilib } */
/* { dg-options "-march=armv7-a -mthumb -O2 -flto -Wa,-mimplicit-it=always" }  */

int main(int x)
{
  asm("teq %0, #0; addne %0, %0, #1" : "=r" (x));
  return x;
}
