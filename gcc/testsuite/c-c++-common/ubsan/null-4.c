/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int
main (void)
{
  _Complex double *p = 0;
  if (p[0])
    return 42;
  return 0;
}

/* { dg-output "load of null pointer of type 'complex double'(\n|\r\n|\r)" } */
