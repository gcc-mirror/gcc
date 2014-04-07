/* Verify that rmw instructions supported */
/* { dg-do assemble } */

int main()
{
  #ifdef __AVR_ISA_RMW__
    __asm("xch Z, r12");
    __asm("las Z, r12");
    __asm("lac Z, r12");
    __asm("lat Z, r12");
  #endif
  return 0;
}
