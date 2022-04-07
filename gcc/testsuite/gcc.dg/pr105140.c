/* { dg-do compile } */
/* { dg-options "-Os -w -Wno-psabi" } */
/* { dg-skip-if "PR105147" { powerpc*-*-* s390*-*-* } } */

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
