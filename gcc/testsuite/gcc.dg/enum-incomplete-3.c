/* PR c/70851 */
/* { dg-do compile } */
/* { dg-options "" } */

enum E e; /* { dg-error "storage size" } */

void bar (int [e]); /* { dg-error "has an incomplete type" } */
void bar2 (int [][e]); /* { dg-error "has an incomplete type" } */

void
foo (void)
{
  int a1[e]; /* { dg-error "has an incomplete type" } */
  int a2[e][3]; /* { dg-error "has an incomplete type" } */

  struct S
  {
    int a3[e]; /* { dg-error "has an incomplete type" } */
  };
}
