/* PR target/100887 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-mavx512f" } */

typedef unsigned __int128 U __attribute__((__vector_size__ (64)));
typedef unsigned __int128 V __attribute__((__vector_size__ (32)));
typedef unsigned __int128 W __attribute__((__vector_size__ (16)));

W
foo (U u, V v)
{
  return __builtin_shufflevector (u, v, 0);
}
