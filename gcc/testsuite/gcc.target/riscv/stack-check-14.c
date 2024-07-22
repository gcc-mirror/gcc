/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

int t1(int);

int t2(int x)
{
  char *p = __builtin_alloca (2048);
  x = t1 (x);
  return p[x];
}


/* This test has a constant sized alloca that is smaller than the
   probe interval.  Only one probe is required since the value is larger
   than 1024 bytes but smaller than page size.

   The form can change quite a bit so we just check for one
   probe without looking at the actual address.  */
/* { dg-final { scan-assembler-times {sd\tzero,1024\(sp\)} 1 } } */



