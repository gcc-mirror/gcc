/* PR c/70851 */
/* { dg-do compile } */
/* { dg-options "" } */

enum E e; /* { dg-error "storage size" } */

void bar (int [e]); /* { dg-error "size of unnamed array has incomplete type" } */
void bar2 (int [][e]); /* { dg-error "size of unnamed array has incomplete type" } */

void
foo (void)
{
  int a1[e]; /* { dg-error "size of array .a1. has incomplete type" } */
  int a2[e][3]; /* { dg-error "size of array .a2. has incomplete type" } */

  struct S
  {
    int a3[e]; /* { dg-error "size of array .a3. has incomplete type" } */
  };
}
