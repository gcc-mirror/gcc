/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  unsigned long int *p = 0;
  *p = 42;
  return 0;
}

/* { dg-output "store to null pointer of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
