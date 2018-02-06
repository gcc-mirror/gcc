/* PR tree-optimization/84228 */
/* { dg-do compile } */
/* { dg-options "-Wstringop-truncation -O2 -g" } */

char *strncpy (char *, const char *, __SIZE_TYPE__);
struct S
{
  char arr[64];
};

int
foo (struct S *p1, const char *a)
{
  int b = 5, c = 6, d = 7;
  if (a)
    goto err;
  strncpy (p1->arr, a, sizeof p1->arr); /* { dg-bogus "specified bound" } */
  b = 8; c = 9; d = 10;
  p1->arr[3] = '\0';
err:
  return 0;
}
