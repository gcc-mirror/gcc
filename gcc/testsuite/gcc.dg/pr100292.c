/* { dg-do compile } */
/* { dg-require-effective-target int32 } */

typedef unsigned char __attribute__((__vector_size__ (4))) V;

extern void bar (V v);

void
foo (char c)
{
  bar (c <= (V) 127);
}
