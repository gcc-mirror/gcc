/* { dg-do compile } */
/* { dg-options "-fsanitize=bounds" } */

struct T { int c; char d[]; } t = { 1, "abcdefg" };

int
baz (int i)
{
  return t.d[i];
}
