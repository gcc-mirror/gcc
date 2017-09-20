/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

extern void arf (unsigned long int *, unsigned long int *);
void
frob ()
{
  unsigned long int num[859];
  unsigned long int den[859];
  arf (den, num);
}

/* { dg-final { scan-assembler-times "subq" 4 } } */
/* { dg-final { scan-assembler-times "orq" 3 } } */

