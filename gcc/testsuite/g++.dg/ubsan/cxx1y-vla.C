/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -w -std=c++1y" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  int y = -18;
  int a[y];
  return 0;
}

/* { dg-output "terminate called after throwing an instance" } */
