/* Test visibility attribute on forward declaration of global variable */
/* { dg-require-visibility "" }
/* { dg-final { scan-hidden "xyzzy" } } */

extern int __attribute__ ((visibility ("hidden")))
xyzzy;

int xyzzy = 5;
