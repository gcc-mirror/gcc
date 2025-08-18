/* { dg-do compile } */
/* PR middle-end/120024 */

void *f(unsigned) __attribute__((malloc));
void g()
{
  void *(*g)(unsigned) = f;
  void (*h)(unsigned) = (void (*)(unsigned))g;
  h(1);
}
