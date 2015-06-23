/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */

void
fn1 (int a)
{
  const int x = a;
  if (x && x) {} /* { dg-warning "logical .and. of equal expressions" } */
  if (x && (int) x) {} /* { dg-warning "logical .and. of equal expressions" } */
  if ((int) x && x) {} /* { dg-warning "logical .and. of equal expressions" } */
  if ((int) x && (int) x) {} /* { dg-warning "logical .and. of equal expressions" } */
}

void
fn2 (int a)
{
  const int x = a;
  if (x || x) {} /* { dg-warning "logical .or. of equal expressions" } */
  if (x || (int) x) {} /* { dg-warning "logical .or. of equal expressions" } */
  if ((int) x || x) {} /* { dg-warning "logical .or. of equal expressions" } */
  if ((int) x || (int) x) {} /* { dg-warning "logical .or. of equal expressions" } */
}
