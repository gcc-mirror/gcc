/* Check that GCC does bti instruction.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -mthumb -mfloat-abi=softfp -mbranch-protection=bti --save-temps" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler "bti" } } */
