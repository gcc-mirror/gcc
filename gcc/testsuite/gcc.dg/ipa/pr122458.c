/* PR ipa/122458 */
/* { dg-do link } */
/* { dg-options "-O2" } */

static int foo (void) { return 0; };

asm (".quad %c0" :: "i" (foo));

int main() {}
