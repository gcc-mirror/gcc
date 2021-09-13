/* PR ipa/99517 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef signed char v32qi __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));
typedef long long int v4di __attribute__((vector_size(32)));
typedef double v4df __attribute__((vector_size(32)));

v32qi
foo (v4si x)
{
  return (v32qi) __builtin_convertvector (x, v4df);
}

v32qi
bar (v4si x)
{
  return (v32qi) __builtin_convertvector (x, v4di);
}
