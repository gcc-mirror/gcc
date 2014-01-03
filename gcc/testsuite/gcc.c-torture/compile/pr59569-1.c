/* PR middle-end/59569 */
extern char c;

void
foo (int i, char **j)
{
  while (i)
    j[--i] = &c;
}
