/* Test visibility attribute on forward declaration of global variable */
/* { dg-do compile { target *86-*-linux* } } */
/* { dg-final { scan-assembler "\\.hidden.*xyzzy" } } */

extern int __attribute__ ((visibility ("hidden")))
xyzzy;

int xyzzy = 5;
