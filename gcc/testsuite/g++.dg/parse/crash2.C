/* { dg-do compile } */
int main(void)
{
  char x, y;
  if ('A' == x) && ('B' == y)) { } /* { dg-error "(parse|syntax) error" } */
  if (x == 'A') && (y == 'B')) { }
}
