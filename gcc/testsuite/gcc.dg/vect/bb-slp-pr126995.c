/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef struct
{
  int x;
  int y;
} cairo_point_t;

typedef struct
{
  cairo_point_t a, b;
} cairo_spline_knots_t;

int _lerp_half_b_1, fallback;
cairo_point_t _de_casteljau_ab, _de_casteljau_abbc;
cairo_spline_knots_t _de_casteljau_s1;
double sq_berr, sq_cdx, sq_cdy;

void
_lerp_half (cairo_point_t *a, cairo_point_t *result)
{
  result->x = a->x + (a->x >> 1);
  result->y = a->y + (_lerp_half_b_1 >> 1);
}

void
_de_casteljau (cairo_spline_knots_t *s2)
{
  cairo_point_t bc, bccd, final;

  _lerp_half (&_de_casteljau_s1.b, &bc);
  _lerp_half (&_de_casteljau_ab, &_de_casteljau_abbc);
  _lerp_half (&bc, &bccd);
  _lerp_half (&_de_casteljau_abbc, &final);

  s2->a = final;
  s2->b = bccd;
}

int
_cairo_spline_decompose_into (cairo_spline_knots_t *s1)
{
  cairo_spline_knots_t s2, knots = *s1;
  sq_cdx = knots.a.x;
  sq_cdy = knots.a.y;
  sq_berr = sq_cdx * sq_cdx + sq_cdy * sq_cdy;

  if (sq_berr)
    return fallback;

  _de_casteljau (&s2);
  _cairo_spline_decompose_into (&s2);

  return 0;
}
