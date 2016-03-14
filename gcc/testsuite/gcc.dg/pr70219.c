/* PR middle-end/70219 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O1 -w -Wno-psabi" } */

typedef int B __attribute__ ((vector_size (32)));
typedef int D __attribute__ ((vector_size (32)));
typedef long E __attribute__ ((vector_size (32)));
typedef __int128 F;
typedef __int128 G __attribute__ ((vector_size (32)));

F
foo (int a, unsigned b, F c, B d, G e, B f, D g, E h, G i)
{
  b /= c;
  e /= (G) ~d;
  h -= (E){ g[4], e[1], 64, ~f[1] };
  return b + e[1] + h[0] + h[1] + i[1];
}
