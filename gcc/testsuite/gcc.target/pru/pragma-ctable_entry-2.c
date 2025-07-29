/* Test for base addresses with bit 31 set (PR121124).  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it LBCO/SBCO operations may
   not be optimized to the respective instructions.  */


#pragma ctable_entry 12 0x80beef00

unsigned int
test_ctable (unsigned int val1, unsigned int val2)
{
  ((volatile unsigned short int *)0x80beef00)[0] = val2;
  ((volatile unsigned int *)0x80beef00)[val1] = val2;
  return ((volatile unsigned int *)0x80beef00)[5];
}

/* { dg-final { scan-assembler "sbco\\tr15.b\[012\]?, 12, 0, 2" } } */
/* { dg-final { scan-assembler "sbco\\tr15.b0, 12, r14, 4" } } */
/* { dg-final { scan-assembler "lbco\\tr14.b0, 12, 20, 4" } } */
