/* Check interwork between static functions for thumb2. */

/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-O0 -march=armv7-a+fp" } */

struct _NSPoint
{
  float x;
  float y;
};

typedef struct _NSPoint NSPoint;

static NSPoint
__attribute__ ((target("arm")))
NSMakePoint (float x, float y)
{
  NSPoint point;
  point.x = x;
  point.y = y;
  return point;
}

static NSPoint
__attribute__ ((target("thumb")))
RelativePoint (NSPoint point, NSPoint refPoint)
{
  return NSMakePoint (refPoint.x + point.x, refPoint.y + point.y);
}

NSPoint
__attribute__ ((target("arm")))
g(NSPoint refPoint)
{
  float pointA, pointB;
  return RelativePoint (NSMakePoint (0, pointA), refPoint);
}

/* { dg-final { scan-assembler-times "blx" 2 } } */
