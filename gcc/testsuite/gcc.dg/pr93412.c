/* PR target/93412 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og" } */

unsigned char a;
int b;
unsigned c;

int
foo (int e, int f, int g, int h, int k, int i, short j)
{
  b = __builtin_add_overflow (a, 0, &c);
  b = __builtin_add_overflow_p (b, a, (unsigned __int128) 0) ? b : 0;
  return e + f + g + a + h + k + i + j + c;
}
