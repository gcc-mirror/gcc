/* PR optimization/10312 */
/* Originator: Peter van Hoof <p dot van-hoof at qub dot ac dot uk> */

/* Verify that the strength reduction pass doesn't find
   illegitimate givs.  */

struct
{
  double a;
  int n[2];
} g = { 0., { 1, 2}};

int k = 0;

void
b (int *j)
{
}

int
main ()
{
  int j;

  for (j = 0; j < 2; j++)
    k = (k > g.n[j]) ? k : g.n[j];

  k++;
  b (&j);

  return 0;
}
