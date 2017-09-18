/* { dg-do compile } */
/* { dg-require-stack-check "" } */
/* { dg-options "-fstack-check -mavx" } */

struct S0
{
  int f0, f1, f2, f3;
} g_106;

struct S0
func_99 ()
{
  return (g_106);
}
