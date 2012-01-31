/* { dg-lto-options {{-flto -fgnu-tm}} } */
/* { dg-lto-do link } */
/* { dg-require-effective-target stdint_types } */

extern void foo() __attribute__((transaction_safe));

int main()
{
  __transaction_atomic {
      foo();
  }
}
