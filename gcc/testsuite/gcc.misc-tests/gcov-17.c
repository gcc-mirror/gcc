/* Test visibility is copied */

/* { dg-options "-fprofile-arcs" } */
/* { dg-require-visibility "" } */
/* { dg-require-weak "" } */

void __attribute__ ((visibility ("hidden"), weak)) Foo ()
{
}

/* { dg-final { scan-assembler "\\.hidden\t__gcov__Foo" } } */
