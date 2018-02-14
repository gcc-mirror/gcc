// { dg-module-do run { target *-*-* } "hello.o" }
// { dg-options "-fmodules" }

// Relies on CXX_MODULE_WRAPPER functionality
import hello;
// { dg-module-bmi "hello" }
int main (void)
{
  greeter ("world");
  return 0;
}

