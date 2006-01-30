/* { dg-do compile { target sh-*-* sh[1234ble]*-*-*} } */
/* { dg-options "-O" } */
/* Check that trapa / interrput_handler attributes can paired in
   either order.  */
void h0() __attribute__ ((trap_exit (4))) __attribute__ ((interrupt_handler));
void h1() __attribute__ ((interrupt_handler)) __attribute__ ((trap_exit (5)));

void foo ()
{
}

void h0 () {}
/* { dg-final { scan-assembler "trapa\[ \t\]\[ \t\]*#4"} } */
/* { dg-final { scan-assembler-times "trapa" 1} } */

void delay(int a)
{
}
int main()
{
  return 0;
}

