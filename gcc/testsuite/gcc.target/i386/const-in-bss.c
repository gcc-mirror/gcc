/* { dg-do compile { target *-*-linux* } } */

__attribute__((section("readonly1"))) const int foo1c;

/* { dg-final { scan-assembler "readonly1,\"a\"" } } */
/* { dg-final { scan-assembler-not "readonly1,\"aw\"" } } */
