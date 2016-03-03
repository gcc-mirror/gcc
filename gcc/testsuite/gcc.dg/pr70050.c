/* PR middle-end/70025 */
/* { dg-do compile } */
/* { dg-options "-Wno-psabi" } */

typedef int v8si __attribute__ ((vector_size (32)));

v8si
foo (v8si v)
{
  return v %= -v;
}
