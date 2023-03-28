/* PR middle-end/106190 */
/* { dg-do compile } */
/* { dg-options "-fnon-call-exceptions -fsanitize=address,undefined -fno-sanitize-recover=all" } */

int
main ()
{
  int a;
  int *b[1];
  int c[10];
  int d[1][1];
  for (a = 0; a < 1; a++)
    d[1][a] = 0;
  return 0;
}
