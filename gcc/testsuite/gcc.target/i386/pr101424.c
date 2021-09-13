/* PR target/101424 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef int v4df __attribute__((vector_size(32)));

int foo_v4df_b, foo_v4df_c;

v4df
__attribute__foo_v4df ()
{
  v4df a;
  a[foo_v4df_c] = foo_v4df_b;
  return a;
}
