/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

typedef volatile const _Complex float *T;

int
main (void)
{
  T t = 0;
  if (*t)
    return 42;
  return 0;
}

/* { dg-output "load of null pointer of type 'volatile const complex float'\[^\n\r]*(\n|\r\n|\r)" } */
