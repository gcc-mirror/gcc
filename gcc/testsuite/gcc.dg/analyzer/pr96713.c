/* { dg-options "-Wno-psabi" } */
typedef int __attribute__ ((vector_size (8))) V;

void
foo (V d, V e)
{
  d <= e;
  foo ((V){}, (V){});
}
