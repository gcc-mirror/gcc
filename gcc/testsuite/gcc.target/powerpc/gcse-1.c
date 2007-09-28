/* { dg-do compile { target { *-*-linux* && ilp32 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "@ha" 1 } } */


/* Test for PR 7003, address of array loaded int register
   twice without any need. */

extern const char flags [256];

unsigned char * f (unsigned char * s) {
  while (flags[*++s]);
  while (!flags[*++s]);
  return s;
}
