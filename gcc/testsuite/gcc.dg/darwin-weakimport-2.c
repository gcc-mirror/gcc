/* { dg-do compile { target *-*-darwin* } } */
/* { dg-require-weak "" } */

/* { dg-final { scan-assembler "weak_reference _foo" } } */

extern int foo __attribute__((weak_import));

int main(void)
{
  if (&foo)
    return foo;
  return 0;
}
