/* { dg-options "-mgp32" } */

extern __thread int x __attribute__ ((tls_model ("initial-exec")));

long long
foo (long long y)
{
  x = 0;
  return y;
}
