/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  _Complex double *p = 0;
  if (p[0])
    return 42;
  return 0;
}

/* { dg-output "load of null pointer of type 'complex double'\[^\n\r]*(\n|\r\n|\r)" } */
