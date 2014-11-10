/* PR middle-end/61529 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

unsigned int a = 0, b = 0;
unsigned int c;

int
main ()
{
  unsigned int d = 0;
  int e[5];

  for (; b < 1; b++)
    d = 0;
  for (; d < 1; d++)
    a = 0;
  for (; a < 1; a++)
    ;

  for (c = 0; c < 5; c++)
    e[c] = 1;
  if (e[0])
    c = 0;

  return 0;
}
