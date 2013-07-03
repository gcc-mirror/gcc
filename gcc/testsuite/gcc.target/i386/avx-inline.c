/* Check if avx target functions can inline lower target functions.   */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-avx -mno-sse3" } */

__attribute__((always_inline,target("sse3")))
inline int callee ()
{
  return 0;
}

__attribute__((target("avx")))
inline int caller ()
{
  return callee ();
}

int main ()
{
  return caller ();
}
