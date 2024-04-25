/* PR target/114416 */
/* Reported by Rainer Orth <ro@gcc.gnu.org> */

/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-effective-target lp64 } */

struct vec2
{
  double x[2];
};

struct vec2x
{
  double x;
  double y;
};

struct vec2 sum2 (double val)
{
  struct vec2 v;
  v.x[0] = val;
  v.x[1] = val;
  return v;
}

struct vec2x sum2x (double val)
{
  struct vec2x v;
  v.x = val;
  v.y = val;
  return v;
}

double get2 (struct vec2 v)
{
  return v.x[0] + v.x[1];
}

double get2x (struct vec2x v)
{
  return v.x + v.y;
}

/* { dg-final { scan-assembler-not "ldx" } } */
/* { dg-final { scan-assembler-not "stx" } } */
