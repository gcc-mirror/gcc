/* This caused an ICE on s390x due to a reload inheritance bug.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

struct point { double x, y; };
extern void use (struct point);

void test (struct point *pc, struct point p1)
{
  struct point p0 = *pc;

  if (p0.x == p1.x && p0.y == p1.y)
    use (p0);

  asm ("" : : : "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10");

  p1.y -= p0.y;

  use (p0);
  use (p1);
}

