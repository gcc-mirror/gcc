/* PR middle-end/83487 */
/* { dg-do compile } */
/* { dg-options "" } */

struct __attribute__ ((__aligned__)) A {};
struct A a;
void bar (int, int, int, int, int, int, int, struct A);

void
foo ()
{
  bar (0, 1, 2, 3, 4, 5, 6, a);
}
