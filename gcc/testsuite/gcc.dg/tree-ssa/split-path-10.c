/* PR tree-optimization/79389  */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-split-paths-details" } */

typedef struct
{
  int m[17];                        
  int seed;                             
  int i;
  int j;
  int haveRange;
  double left;
  double right;
  double width;
}
Random_struct, *Random;

Random new_Random_seed(int seed);
double Random_nextDouble(Random R);
void Random_delete(Random R);

static const int SEED = 113;

double MonteCarlo_integrate(int Num_samples)
{


  Random R = new_Random_seed(SEED);


  int under_curve = 0;
  int count;

  for (count=0; count<Num_samples; count++)
    {
      double x= Random_nextDouble(R);
      double y= Random_nextDouble(R);

      if ( x*x + y*y <= 1.0)
	under_curve ++;

    }

  Random_delete(R);

  return ((double) under_curve / Num_samples) * 4.0;
}

/* { dg-final { scan-tree-dump-times "Duplicating join block" 0 "split-paths" } } */
