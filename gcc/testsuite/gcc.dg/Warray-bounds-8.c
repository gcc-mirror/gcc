/* { dg-do compile } */
/* { dg-options "-O3 -Wall" } */
/* based on PR 43833 */

extern unsigned char data[5];

unsigned char
foo (char *str)
{
  int i, j;
  unsigned char c = 0;

  for (i = 0; i < 8; i++)
    {
      j = i * 5;
      if ((j % 8) > 3)
	c |= data[(j / 8) + 1];
    }
  return c;
}
