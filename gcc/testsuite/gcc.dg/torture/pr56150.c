/* { dg-do compile } */

struct {
    int f4;
} g1;

long g2;

volatile long g3;

void func_1 ()
{
  if (g2)
    g1 = g1;
  else
    g3;
}
