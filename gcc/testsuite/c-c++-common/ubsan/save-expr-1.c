/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -Wall -Werror -O -fno-sanitize-recover" } */

static int x;
int
main (void)
{
  int o = 1;
  int y = x << o;
  return y;
}
