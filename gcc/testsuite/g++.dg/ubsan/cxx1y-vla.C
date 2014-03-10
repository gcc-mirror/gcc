/* { dg-do run { target c++1y } } */
/* { dg-options "-fsanitize=vla-bound -Wall -Wno-unused-variable" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  int y = -18;
  int a[y];
  return 0;
}

/* { dg-output "terminate called after throwing an instance" } */
