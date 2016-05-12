/* PR c/70756 */
/* { dg-do compile } */
/* { dg-options "" } */

enum E e; /* { dg-error "storage size|use of enum" } */
int (*A)[];

void
fn0 (void)
{
  struct
  {
    int x;
    int y[];
  } s;
  1234 && &s.y + 1; /* { dg-error "16:invalid use of" } */
}

void
fn1 (void)
{
  1234, A += 1; /* { dg-error "11:invalid use of array with unspecified bounds" } */
}
