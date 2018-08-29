/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -mtune=generic -fdump-rtl-expand" } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */

int f1 (char *);

int
f2 (void)
{
  const int size = 4096;
  char buffer[size];
  return f1 (buffer);
}

/* So we want to verify that at expand time that we probed the main
   VLA allocation as well as the residuals.  Then we want to verify
   there was only one probe in the final assembly (implying the
   residual probe was optimized away).  */
/* { dg-final { scan-rtl-dump-times "allocation and probing in loop" 1 "expand" } } */
/* { dg-final { scan-rtl-dump-times "allocation and probing residuals" 1 "expand" } } */

/* { dg-final { scan-assembler-times "or\[ql\]" 1 } } */

