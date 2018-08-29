/* PR lto/84212 - -Wno-stringop-verflow does not disable warnings from
   -flto link stage  */

extern void clear (char*, unsigned);

int main (void)
{
  char x[3];

  clear (x, 4);
}
