// { dg-module-do run { target *-*-* } "main-aux.s" }
// Relies on default oracle functionality
// { dg-additional-options -fmodules-ts }

import main.aux;
// { dg-module-bmi "main.aux" }

int main (void)
{
  greeter ("world");
  return 0;
}

