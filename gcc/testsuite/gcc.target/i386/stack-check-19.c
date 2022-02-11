/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic -fdump-rtl-expand -fno-stack-protector" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */

int f1 (char *);

int
f2 (const int size)
{
  char buffer[size];
  return f1 (buffer);
}

/* So we want to verify that at expand time that we probed the main
   VLA allocation as well as the residuals.  Then we want to verify
   there are two probes in the final assembly code.  */
/* { dg-final { scan-rtl-dump-times "allocation and probing in loop" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "allocation and probing residuals" 1 "expand" } } */
/* { dg-final { scan-assembler-times "or\[ql\]" 2 } } */

/* We also want to verify (indirectly) that the residual probe is
   guarded.  We do that by checking the number of conditional
   branches.  There should be 3.  One that bypasses the probe loop, one
   in the probe loop and one that bypasses the residual probe.

   These will all be equality tests.  */
/* { dg-final { scan-assembler-times "(\?:je|jne)" 3 } } */


