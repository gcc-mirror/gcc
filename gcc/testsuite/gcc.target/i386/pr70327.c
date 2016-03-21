/* PR target/70327 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mavx512f" } */

typedef unsigned __int128 v4ti __attribute__ ((vector_size (64)));

void
foo (v4ti v)
{
  foo(v);
}
