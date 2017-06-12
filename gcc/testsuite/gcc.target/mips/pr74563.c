/* { dg-do compile } */
/* { dg-options "-mips3 -mips16 -msoft-float" } */

void f2(void);

void f1(void)
{
        f2();
}

/* { dg-final { scan-assembler-not "\tjr\t\\\$31" } } */
/* { dg-final { scan-assembler "\tjr\t\\\$7" } } */


