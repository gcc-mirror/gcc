foo()
{
      char d;
      asm volatile ( "" :: "m"(&d));
}
