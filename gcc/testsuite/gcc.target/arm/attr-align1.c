/* PR target/67745
   Verify alignment when both attribute optimize and target are used.  */
/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-skip-if "" arm_cortex_m } */

void
__attribute__ ((target ("arm")))
bar()
{
}

void
__attribute__ ((target ("thumb")))
__attribute__ ((optimize ("Os")))
foo()
{
}

void
__attribute__ ((target ("thumb")))
__attribute__ ((optimize ("O2")))
rab()
{
}

/* { dg-final { scan-assembler-times ".align\[ \t]2" 2 } } */
/* { dg-final { scan-assembler ".align\[ \t]1" } } */
