/* { dg-do compile { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O0" } */

float foo ()
{
  float ret;
  asm ("fld\t%H0,(a0)\n\t":"=f"(ret));

  return ret;
}

/* { dg-error "modifier 'H' is for integer registers only" "" { target { "riscv*-*-*" } } 0 } */
