/* PR middle-end/67222 */
/* { dg-do compile } */

void
foo (void **p)
{
  posix_memalign (); /* { dg-warning "implicit declaration" } */
  posix_memalign (p);
  posix_memalign (0);
  posix_memalign (p, 1);
  posix_memalign (p, "foo");
  posix_memalign ("gnu", "gcc");
  posix_memalign (1, p);
  posix_memalign (1, 2);
  posix_memalign (1, 2, 3);
  posix_memalign (p, p, p);
  posix_memalign (p, "qui", 3);
  posix_memalign (p, 1, 2);
}
