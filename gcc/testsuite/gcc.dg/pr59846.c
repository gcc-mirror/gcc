/* PR c/59846 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op -Wtype-limits" } */

_Bool
fn1 (unsigned int p)
{
  return 0UL > p; /* { dg-warning "14:comparison is always false due to limited range of data type" } */
}

_Bool
fn2 (unsigned int p)
{
  return 0UL <= p; /* { dg-warning "14:comparison is always true due to limited range of data type" } */
}

_Bool
fn3 (unsigned int p)
{
  return p >= 0U; /* { dg-warning "12:comparison of unsigned expression >= 0 is always true" } */
}

_Bool
fn4 (unsigned int p)
{
  return p < 0U; /* { dg-warning "12:comparison of unsigned expression < 0 is always false" } */
}

_Bool
fn5 (_Bool p)
{
  return p || !p; /* { dg-warning "12:logical" } */
}

_Bool
fn6 (_Bool p)
{
  return p && !p; /* { dg-warning "12:logical" } */
}
