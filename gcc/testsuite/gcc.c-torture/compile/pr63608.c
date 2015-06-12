/* PR middle-end/63608 */

typedef long T;
typedef unsigned long U;
unsigned long a;

unsigned long
foo (int b)
{
  T c = 0;
  const U d = 2248593032UL;
  a = (c = +d) | (~4L & ~b);
  return c;
}
