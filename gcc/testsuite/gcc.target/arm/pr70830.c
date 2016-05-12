/* PR target/70830.  */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-options "-Os -marm -save-temps" } */

/* This test is not valid when -mthumb.  */

extern void prints (char *);

void __attribute__ ((interrupt ("IRQ"))) dm3730_IRQHandler(void)
{
    prints("IRQ" );
}
/* { dg-final { scan-assembler "ldmfd\tsp!, {r0, r1, r2, r3, ip, pc}\\^" } } */
