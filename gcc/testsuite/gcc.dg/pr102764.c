/* PR middle-end/102764 */
/* Reported by Chengnian Sun <cnsun@uwaterloo.ca> */

/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */

volatile int a;

void main (void)
{
  for (int i = 0; i < 1000; i++)
    if (i % 17)
      a++;
}
