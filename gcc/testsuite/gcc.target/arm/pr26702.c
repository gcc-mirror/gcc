/* { dg-do compile { target arm*-*-eabi* } } */
/* { dg-final { scan-assembler "\\.size\[\\t \]+static_foo, 4" } } */
int foo;
static int static_foo;
