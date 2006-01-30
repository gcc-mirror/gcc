/* { dg-do compile { target h8300-*-* sh-*-* sh[1234ble]*-*-*} } */
/* { dg-options "-O" } */
/* This test case will check whether rte is generated only for isr.  */
#pragma interrupt
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

/* { dg-final { scan-assembler-times "rte" 1} } */
