/* PR target/67745
   Verify alignment when attribute optimize is used.  */
/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-O2 -mthumb" }  */

/* Check that thumb code is always 2 bytes aligned for -Os.  */

void
__attribute__ ((optimize("Os")))
foo()
{
}

/* { dg-final { scan-assembler ".align\[ \t]1" } } */
