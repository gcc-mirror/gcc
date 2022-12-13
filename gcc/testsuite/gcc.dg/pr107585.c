/* PR target/107585 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned char __attribute__((__vector_size__ (16))) V;
char c;
void bar (int);

void
foo (void)
{
  bar (((V) (c <= (V){127}))[2]);
}
