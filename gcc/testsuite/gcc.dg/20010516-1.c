foo()
{
      char d;
      __asm volatile ( "" :: "m"(&d)); /* { dg-error "" "non-lvalue" } */
}
