/* { dg-do compile { target arm_eabi } } */
/* { dg-final { scan-assembler "\\.size\[\\t \]+static_foo, 4" } } */
int foo;
static int static_foo;
