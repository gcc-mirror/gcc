/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wunknown-pragmas" } */

/* Tests the clauses in several combinations put in different locations.  */
/* This is mostly a parser test.  */
#define Q 4

int z = Q;

 __attribute__ ((vector (uniform(x), linear (y:1), vectorlength (4) )))
int func (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}
 __attribute__ ((__vector__ (uniform(x), vectorlength (2), linear (y:1) )))
int func2 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(y), linear (x), vectorlength (4) )))
int func3 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), mask)))
int func4 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), linear (y:1), nomask)))
int func5 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform(x), mask, linear (y:1)))) 
int func6 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform (x), mask, linear (y:1)), vector))
int func7 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector (uniform (x), mask, linear (y:1)), vector (uniform (y), mask)))
int func8 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

__attribute__ ((vector, vector (uniform (y), mask)))
int func9 (int x, int y)
{
  int zq = 5;
  return x + (y*zq);
}

int main (int argc, char *argv[])
{
  int ii = 0, q = 5;
  for (ii = 0; ii < 10; ii++)
    q += func (argc, ii);
  return q;
}
