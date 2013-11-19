/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int *
gao (void)
{
  return 0;
}

int
main (void)
{
  return *gao ();
}

/* { dg-output "load of null pointer of type 'int'(\n|\r\n|\r)" } */
