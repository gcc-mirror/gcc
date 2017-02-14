/* PR c/71583 */
/* { dg-do compile } */

void
f (int i)
{
  (int (*)[++i]) { int }; /* { dg-error "expected" } */
  (int (*)[++i]) { }; /* { dg-error "empty" } */
  (int (*)[++i]) { , }; /* { dg-error "expected" } */
  (int (*)[++i]) { f () }; /* { dg-error "too few" } */
}
