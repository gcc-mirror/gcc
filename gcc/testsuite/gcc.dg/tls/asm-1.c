/* { dg-options "-Werror" } */
__thread int i;

int foo ()
{
  asm volatile ("" :: "m" (&i));	/* { dg-error "lvalue" } */
}
