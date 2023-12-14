/* { dg-additional-options "-Wno-overflow" } */

struct yc {
  int c0;
  char di[];
};

void
qt (struct yc *ab)
{
  ab->di[0x7fffffff + 1] = ab->di[0];
}
