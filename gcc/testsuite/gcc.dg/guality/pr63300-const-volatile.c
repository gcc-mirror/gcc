/* PR63300 'const volatile' sometimes stripped in debug info */
/* { dg-do run } */
/* { dg-options "-g" } */

int
main (int argc, char **argv)
{
  const volatile int v = argc;
  return v - argc;
}

/* { dg-final { gdb-test 9 "type:v" "const volatile int" } } */
