/* PR 113148: ICE caused by infinite reloading */
/* { dg-do compile } */
/* { dg-options "-O2 -march=la464 -mfpu=64 -mabi=lp64d" } */

struct bound
{
  double max;
} drawQuadrant_bound;
double w4, innerXfromXY_y, computeBound_right_0;
struct arc_def
{
  double w, h;
  double a0, a1;
};
static void drawQuadrant (struct arc_def *);
static void
computeBound (struct arc_def *def, struct bound *bound)
{
  double ellipsex_1, ellipsex_0;
  bound->max = def->a1 ?: __builtin_sin (w4) * def->h;
  if (def->a0 == 5 && def->w == def->h)
    ;
  else
    ellipsex_0 = def->a0 == 0.0 ?: __builtin_cos (w4);
  if (def->a1 == 5 && def->w == def->h)
    ellipsex_1 = bound->max;
  __builtin_sqrt (ellipsex_1 * innerXfromXY_y * innerXfromXY_y * w4);
  computeBound_right_0 = ellipsex_0;
}
void
drawArc ()
{
  struct arc_def foo;
  for (;;)
    drawQuadrant (&foo);
}
void
drawQuadrant (struct arc_def *def)
{
  int y, miny;
  computeBound (def, &drawQuadrant_bound);
  while (y >= miny)
    ;
}
