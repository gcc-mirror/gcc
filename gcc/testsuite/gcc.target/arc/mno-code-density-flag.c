/* { dg-do assemble } */
/* { dg-require-effective-target codedensity } */
/* { dg-skip-if "" { ! { arcem } } } */
/* { dg-options "-mno-code-density" } */

void
f (void)
{
  __asm__ volatile ("enter_s {r13-r13}");
}

/* { dg-message "Error: opcode 'enter_s' not supported" "" { target *-*-* } 0 } */
