/* PR optimization/11662 */
/* Origin: heinrich.brand@fujitsu-siemens.com */

/* This used to fail on SPARC at -O1 because the combiner didn't
   correctly propagate an error indicator.  */

unsigned long long r;

void test(unsigned long a, unsigned long b, unsigned long long c)
{
  r = (a^b)&c;
}

int main()
{
  test(1,2,3);

  if (r != 3)
    abort();

  return 0;
} 
