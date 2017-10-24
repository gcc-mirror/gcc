// { dg-module-do run }

// Relies on CXX_MODULE_WRAPPER functionality
import hello;
// { dg-module-bmi "hello" }
int main (void)
{
  greeter ("world");
  return 0;
}

