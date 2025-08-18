/* { dg-do compile } */
/* PR middle-end/120024 */

void *f(unsigned) __attribute__((malloc));
void h1(void);
void g(void)
{
  void *(*g)(unsigned) = f;
  void (*h)(unsigned) = (void (*)(unsigned))g;
  h(1);
  h1();
}
