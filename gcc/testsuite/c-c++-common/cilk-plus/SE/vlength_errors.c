/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wunknown-pragmas" } */

#define Q 4

int z = Q;

__attribute__ ((vector (uniform(x), vectorlength (), linear (y:1) ))) /* { dg-error "expected expression" } */
int func2 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), vectorlength (4.5) ))) /* { dg-error "vectorlength must be an integer" } */
int func3 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), vectorlength (z) ))) /* { dg-error "vectorlength must be an integer" } */
int func4 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), vectorlength (Q) ))) /* This is OK!  */
int func5 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), vectorlength (z), linear (y:1)))) /* { dg-error "vectorlength must be an integer" } */
int func6 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), vectorlength (sizeof (int)) ))) /* This is OK too!  */
int func7 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

int main (void)
{
  int ii = 0, q = 5;
  for (ii = 0; ii < 10; ii++)
    q += func2 (z, ii);
  return q;
}
