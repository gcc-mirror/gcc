/* Test -no_dead_strip_inits_and_terms support. */
/* Contributed by Devang Patel  <dpatel@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-no_dead_strip_inits_and_terms" } */

int
main ()
{
  return 0;
}

