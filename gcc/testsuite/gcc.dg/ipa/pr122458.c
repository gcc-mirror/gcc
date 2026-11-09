/* PR ipa/122458 */
/* { dg-do link } */
/* { dg-options "-O2 -no-pie -fno-pie" } */
/* { dg-xfail-if ".dc.a is a gas extension" { ! gas } } */

static int foo (void) { return 0; };

asm (".dc.a %c0" :: "i" (foo));

int main() {}
