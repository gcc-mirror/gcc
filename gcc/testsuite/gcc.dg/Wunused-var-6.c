/* PR c/99588 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wunused-but-set-variable" } */

void bar (int, ...);
struct S { int a, b, c; };
typedef _Atomic struct S T;

void
foo (void)
{
  static T x = (struct S) { 0, 0, 0 };	/* { dg-bogus "set but not used" } */
  bar (0, x = (struct S) { 1, 1, 1 });
}
