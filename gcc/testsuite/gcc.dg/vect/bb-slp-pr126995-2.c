/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

typedef enum
{
  CAIRO_STATUS_LAST_STATUS
} cairo_status_t;
typedef struct
{
  int x;
  int y;
} cairo_point_t;

typedef struct
{
  cairo_point_t a, b, c, d;
} cairo_spline_knots_t;

double _cairo_spline_decompose_into_tolerance_squared;

static void
_lerp_half (cairo_point_t *a, cairo_point_t *b, cairo_point_t *result)
{
  result->x = a->x + (b->x - a->x >> 1);
  result->y = a->y + (b->y - a->y >> 1);
}

static void
_de_casteljau (cairo_spline_knots_t *s1, cairo_spline_knots_t *s2)
{
  cairo_point_t ab, bc, cd, abbc, bccd, final;
  _lerp_half (&s1->a, &s1->b, &ab);
  _lerp_half (&s1->b, &s1->c, &bc);
  _lerp_half (&ab, &bc, &abbc);
  _lerp_half (&bc, &cd, &bccd);
  _lerp_half (&abbc, &bccd, &final);
  s2->a = final;
  s2->b = s2->c = cd;
  s1->c = abbc;
  s1->d = final;
}

static double
_cairo_spline_error_squared (cairo_spline_knots_t *knots)
{
  double cdy = knots->a.y;
  cdy = 0;
  if (knots->a.x != knots->d.x || knots->d.y)
    {
      double dx, dy, u, v;
      dx = (double)(knots->d.x - knots->a.x);
      dy = (double)(knots->d.y - knots->a.y);
      v = dx * dx + dy * dy;
      u = 0 * dx * dy;
      if (u <= 0)
        cdy -= v * dy;
    }
  if (knots->a.y > cdy)
    return knots->a.y;
  return 0;
}

cairo_status_t
_cairo_spline_decompose_into (cairo_spline_knots_t *s1)
{
  cairo_spline_knots_t s2;
  if (_cairo_spline_error_squared (s1)
      < _cairo_spline_decompose_into_tolerance_squared)
    return 0;
  _de_casteljau (s1, &s2);
  _cairo_spline_decompose_into (s1);
  _cairo_spline_decompose_into (&s2);
  return 0;
}
