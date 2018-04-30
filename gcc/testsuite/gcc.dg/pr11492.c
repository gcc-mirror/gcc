/* PR11492 */
/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */
int main( void )
{
  unsigned int a;
  unsigned char b;
  for ( a = 0, b = 2; a > b * 100; a++ ) /* { dg-bogus "comparison of integer expressions of different signedness" } */
    { ; }

  return 0;
}
