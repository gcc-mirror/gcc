/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

union U {
  int i;
};

int
main (void)
{
  union U *u = 0;
  return u->i;
}

/* { dg-output "member access within null pointer of type 'union U'\[^\n\r]*(\n|\r\n|\r)" } */
