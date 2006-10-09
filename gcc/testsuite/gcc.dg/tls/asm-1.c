/* { dg-options "-Werror" } */
/* { dg-require-effective-target tls } */
__thread int i;

int foo ()
{
  asm volatile ("" :: "m" (&i));  /* { dg-error "directly addressable" } */
}
