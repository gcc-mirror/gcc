/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int
main (void)
{
  int *p = 0;
  return *p;
}

/* { dg-output "load of null pointer of type 'int'(\n|\r\n|\r)" } */
