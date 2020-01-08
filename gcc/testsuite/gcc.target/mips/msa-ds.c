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

/* { dg-final { scan-assembler-not "foo:.*nop.*jr.*foo," } } */
/* { dg-final { scan-assembler-not "bar:.*nop.*jr.*bar," } } */
