/* PR middle-end/70992 */
/* { dg-do compile } */

typedef unsigned int uint32_t;
typedef int int32_t;

uint32_t
fn (uint32_t so)
{
  return (so + so) * (0x80000000 / 0 + 1); /* { dg-warning "division by zero" } */
}

uint32_t
fn5 (uint32_t so)
{
  return (0x80000000 / 0 + 1) * (so + so); /* { dg-warning "division by zero" } */
}

uint32_t
fn6 (uint32_t so)
{
  return (0x80000000 / 0 - 1) * (so + so); /* { dg-warning "division by zero" } */
}

uint32_t
fn2 (uint32_t so)
{
  return (so + so) * (0x80000000 / 0 - 1); /* { dg-warning "division by zero" } */
}

int32_t
fn3 (int32_t so)
{
  return (so + so) * (0x80000000 / 0 + 1); /* { dg-warning "division by zero" } */
}

int32_t
fn4 (int32_t so)
{
  return (so + so) * (0x80000000 / 0 - 1); /* { dg-warning "division by zero" } */
}
