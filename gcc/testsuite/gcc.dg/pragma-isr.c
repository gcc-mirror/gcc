/* { dg-do compile { target h8300-*-* sh-*-* sh[1234ble]*-*-*} } */
/* { dg-options "-O3" } */
/* Test case will check whether rte is generated for two ISRs*/
extern void foo();
#pragma interrupt
void  isr1(void)
{
		foo();
}

#pragma interrupt
void  isr2(void)
{
		foo();
}

/* { dg-final { scan-assembler-times "rte" 2} } */
