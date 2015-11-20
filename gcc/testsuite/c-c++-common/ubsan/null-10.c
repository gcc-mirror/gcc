/* { dg-do run } */
/* { dg-options "-fsanitize=null -w" } */
/* { dg-shouldfail "ubsan" } */

short x;

int
main (void)
{
  short *p = 0, *u = &x;
  *(u + *p) = 23;
  return  0;
}

/* { dg-output "load of null pointer of type 'short int'" } */
