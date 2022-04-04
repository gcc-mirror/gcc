/* { dg-do compile } */
/* { dg-options "-Os -w -Wno-psabi" } */

typedef char __attribute__((__vector_size__ (16 * sizeof (char)))) U;
typedef int __attribute__((__vector_size__ (16 * sizeof (int)))) V;

void bar ();

bar (int i, int j, int k, V v)
{
}

void
foo (void)
{
  bar ((V){}, (V){}, (V){}, (U){});
}
