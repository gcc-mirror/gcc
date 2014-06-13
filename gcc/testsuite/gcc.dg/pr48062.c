/* PR c/48062 */
/* { dg-do compile } */
/* { dg-options "-Wshadow" } */

int
main (void)
{
  int i; /* { dg-bogus "shadowed declaration" } */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
  { int i; }
#pragma GCC diagnostic pop
}
