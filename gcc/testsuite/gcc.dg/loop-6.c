/* PR optimization/18577 */
/* Origin: Falk Hueffner <falk@debian.org> */

/* { dg-do run } */
/* { dg-options "-O2 -funroll-all-loops" } */

static float tfcos12[3];
__attribute__((noinline)) double f(double x) { return x; }
int g;

int main(void)
{
  int i, j;
  for (i = 0; i < 1; i++) 
    tfcos12[i] = 0.5;
    
  for (i = 0; i < 1; i++)
    {
      tfcos12[i] = 0.5 * f(i);
      for (j = 0; j < 12; j++)
	g++;
    }

  return 0;
}
