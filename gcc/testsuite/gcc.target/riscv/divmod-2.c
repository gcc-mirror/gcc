/* { dg-do compile } */
/* Skip this everywhere for now.  Once we have a target with
   divmod enabled, only skip for -O0, -O1, -Og, -Oz, -Os.  */
/* { dg-skip-if "" { *-*-* } { } } */

void
foo(int a, int b, int *c, int *d)
{
   *c = a / b;
   *d = a % b;
}

/* { dg-final { scan-assembler-not "\trem" } } */
/* { dg-final { scan-assembler-times "\tdiv" 1 } } */
/* { dg-final { scan-assembler-times "\tmul" 1 } } */
/* { dg-final { scan-assembler-times "\tsub" 1 } } */
