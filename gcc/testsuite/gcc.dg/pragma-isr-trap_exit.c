/* { dg-do compile { target sh-*-* sh[1234ble]*-*-*} } */
/* { dg-options "-O" } */
/* This test case will check whether trapa is generated only for isr.  */
#pragma interrupt
void isr() __attribute__ ((trap_exit (4)));
void isr()
{
}
void delay(int a)
{
}
int main()
{
  return 0;
}

/* { dg-final { scan-assembler-times "trapa\[ \t\]\[ \t\]*#4" 1} } */
