/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

int t1(int);

int t2(int x)
{
  char *p = __builtin_alloca (4050);
  x = t1 (x);
  return p[x];
}


/* This test has a constant sized alloca that is smaller than the
   probe interval.  Only one probe is required since the value is larger
   than 1024 bytes but smaller than 63k.

   The form can change quite a bit so we just check for two
   probes without looking at the actual address.  */
/* { dg-final { scan-assembler-times "str\\txzr," 1 } } */



