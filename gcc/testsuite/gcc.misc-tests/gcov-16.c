/* Test visibility is copied */

/* { dg-options "-fprofile-arcs -fvisibility=hidden" } */
/* { dg-require-visibility "" } */
/* { dg-require-weak "" } */

void Foo ()
{
}

 /* { dg-final { scan-assembler "\\.hidden\t__gcov__Foo" } } */
