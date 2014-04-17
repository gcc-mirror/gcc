/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  unsigned int len = 1;
  float (*P)[len][len] = 0;
  (*P)[0][0] = 1;
  return 0;
}

/* { dg-output "store to null pointer of type 'float'\[^\n\r]*(\n|\r\n|\r)" } */
