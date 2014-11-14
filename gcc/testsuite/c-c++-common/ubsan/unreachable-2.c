/* { dg-do run } */
/* { dg-options "-fsanitize=unreachable" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-shouldfail "ubsan" } */

int e;

int
main (void)
{
  return e ? 0 : (__builtin_unreachable (), 1);
}

/* { dg-output "execution reached a __builtin_unreachable\\(\\) call" } */
