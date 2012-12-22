/* { dg-do run } */
/* { dg-options "-O -fno-split-wide-types" } */

typedef struct
{
  long int p_x, p_y;
} Point;

static __attribute__ ((noinline, noclone))
     void foo (Point p0, Point p1, Point p2, Point p3)
{
  if (p0.p_x != 1
      || p1.p_x != 3
      || p2.p_x != 5
      || p3.p_x != 7)
    __builtin_abort ();
}

int
main (int argc, char *argv[])
{
  Point p0, p1, p2, p3, p4, p5;
  p0.p_x = 1;
  p1.p_x = 3;
  p2.p_x = 5;
  p3.p_x = 7;
  foo (p0, p1, p2, p3);
  return 0;
}
