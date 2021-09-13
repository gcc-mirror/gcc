/* { dg-require-alias "" } */
/* { dg-require-weak "" } */
/* { dg-xfail-if "weak alias" { powerpc-ibm-aix* } } */

static int dummy = 0;
extern int foo __attribute__((__weak__, __alias__("dummy")));
typedef char check[2*!__builtin_constant_p(dummy)-1];
typedef char check[2*!__builtin_constant_p(foo)-1];
