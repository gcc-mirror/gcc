/* PR c/68412 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra" } */

#define M (sizeof (int) * __CHAR_BIT__)

int
fn1 (int i)
{
  return i == (-1 << 8); /* { dg-warning "left shift of negative value" } */
}

int
fn2 (int i)
{
  return i == (1 << M); /* { dg-warning "left shift count" } */
}

int
fn3 (int i)
{
  return i == 10 << (M - 1); /* { dg-warning "requires" } */
}

int
fn4 (int i)
{
  return i == 1 << -1; /* { dg-warning "left shift count" } */
}

int
fn5 (int i)
{
  return i == 1 >> M; /* { dg-warning "right shift count" } */
}

int
fn6 (int i)
{
  return i == 1 >> -1; /* { dg-warning "right shift count" } */
}
