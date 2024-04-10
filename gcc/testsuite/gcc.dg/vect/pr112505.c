/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

short int w9;
struct T {
  short a : 14;
  int b;
};
struct T v;
void zc()
{
  for(int i = 0; i < 4; i ++)
    w9 *= v.b ? v.a-- < 0 : 0;
}
