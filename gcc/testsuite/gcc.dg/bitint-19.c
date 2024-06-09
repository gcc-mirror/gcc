/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu23" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

void
foo (_Complex int ci, _Complex long long cl)
{
  _BitInt(__SIZEOF_INT__ * __CHAR_BIT__ - 1) bi = 0wb;
  _BitInt(__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1) bl = 0wb;
  static_assert (expr_has_type (ci + bi, _Complex int));
  static_assert (expr_has_type (cl + bl, _Complex long long));
  static_assert (expr_has_type (bi + ci, _Complex int));
  static_assert (expr_has_type (bl + cl, _Complex long long));
}
