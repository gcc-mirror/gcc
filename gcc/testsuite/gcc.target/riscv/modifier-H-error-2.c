/* { dg-do compile { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O0 " } */

void foo ()
{
  register int x31 __asm__ ("x31");
  asm ("li\t%H0,1\n\t":"=r"(x31));
}

/* { dg-error "modifier 'H' cannot be applied to R31" "" { target { "riscv*-*-*" } } 0 } */
