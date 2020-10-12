/* PR middle-end/96335 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (int, void *) __attribute__((__access__(__read_only__, 2)));

void
foo (void *x)
{
  void (*fn) () = bar;
  fn (0, x);
}
