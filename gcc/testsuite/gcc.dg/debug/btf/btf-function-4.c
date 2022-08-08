/* Test BTF linkage for functions.

   We expect to see one BTF_KIND_FUNC type with static linkage encoded in the
   BTF type's vlen field.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "btt_info: kind=12, kflag=0, linkage=0" 1 } } */

static int funfoo (void)
{
  return 0;
}
