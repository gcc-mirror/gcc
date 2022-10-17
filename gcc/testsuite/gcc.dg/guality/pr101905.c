/* { dg-do run { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-g -ffixed-r15" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

register unsigned long long regVar asm ("r15");

int
main()
{
  regVar = 0xdeadbeefcafebabeULL;
  asm ("nop" : "+r" (regVar));
  asm volatile ("nop");		/* { dg-final { gdb-test . "regVar" "0xdeadbeefcafebabeULL" } } */
  asm volatile ("nop" : : "r" (regVar));
  return 0;
}
