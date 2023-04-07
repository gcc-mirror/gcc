/* { dg-options "-mmsa -mfp64 -mhard-float" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* Check that delay slots for MSA branches are filled. */

typedef unsigned v4si __attribute__ ((vector_size (16)));

int __attribute__ ((cold)) foo (v4si v , int a, int b)
{
  int c = 0xf0f0f0f0;
  int f = __builtin_msa_bnz_w (v);

  if (f)
   return a + c;
  else
   return b + c;
}

int __attribute__ ((cold)) bar (v4si v , int a, int b)
{
  int c = 0xf0f0f0f0;
  int f = __builtin_msa_bz_w (v);

  if (f)
   return a + c;
  else
   return b + c;
}

/* We need to avoid over matching here as we could have other
   branches with unfilled slots.  So we verify that we do not have
   an MSA branch with a NOP in its delay slot.  We need to match
   both forms of the MSA branch that can occur in this test.  */
/* { dg-final { scan-assembler-not "foo:.*bn\?z.w\[^\\n\\r\]*\\n\\tnop" } } */
/* { dg-final { scan-assembler-not "bar:.*bn\?z.w\[^\\n\\r\]*\\n\\tnop" } } */
