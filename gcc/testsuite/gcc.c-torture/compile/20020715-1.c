/* PR optimization/7153 */
/* Verify that GCC doesn't promote a register when its
   lifetime is not limited to one basic block. */

void f(char);
void g(void);

void scale(void)
{
  int width;
  char bytes;
  char *src;

  if (width)
  {
    bytes = *src;
    g();
    width *= bytes;
  }

  f(bytes);
}
