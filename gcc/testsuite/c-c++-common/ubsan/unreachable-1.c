/* { dg-do run } */
/* { dg-options "-fsanitize=unreachable" } */
/* { dg-shouldfail "ubsan" } */

int
main (void)
{
  __builtin_unreachable ();
}
/* { dg-output "execution reached an unreachable program point" } */
