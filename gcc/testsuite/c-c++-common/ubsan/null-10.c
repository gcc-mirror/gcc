/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

int
main (void)
{
  short *p = 0, *u;
  *(u + *p) = 23;
  return  0;
}

/* { dg-output "load of null pointer of type 'short int'(\n|\r\n|\r)" } */
