/* { dg-do compile } */

/* PR middle-end/106982 */

void test1(double *c)
{
    double reduced[5];
#pragma acc parallel loop gang private(reduced)
    for (int x = 0; x < 5; ++x)
#pragma acc loop worker reduction(*:reduced)
      for (int y = 0; y < 5; ++y) { }
}
