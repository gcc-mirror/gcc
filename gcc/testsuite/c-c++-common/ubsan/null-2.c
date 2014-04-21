/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  int ***ppp = 0;
  return ***ppp;
}

/* { dg-output "load of null pointer of type 'int \\*\\*'\[^\n\r]*(\n|\r\n|\r)" } */
