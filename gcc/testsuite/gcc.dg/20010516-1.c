/* { dg-options "-fshow-column" } */

foo()
{
      char d;
      __asm volatile ( "" :: "m"(&d)); /* { dg-error "34:" "non-lvalue" } */
}
