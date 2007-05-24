/* { dg-do-compile } */
/* { dg-options "-O0 -fsplit-wide-types" } */

long long int segfault (long long int a, long long int b)
{
  return a ^ b;
}
