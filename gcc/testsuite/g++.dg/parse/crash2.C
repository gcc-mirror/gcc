/* { dg-do compile } */
int main(void)
{
  char x, y;
  if ('A' == x) && ('B' == y)) { } /* { dg-error "" } */
  if (x == 'A') && (y == 'B')) { } /* { dg-error "" } */
}
