/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  int *p = 0;
  return *p;
}

/* { dg-output "load of null pointer of type 'int'(\n|\r\n|\r)" } */
