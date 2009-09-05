/* { dg-do compile } */
/* { dg-options "-g" } */

long *H2_ipX_ener_sort;
double H2_old_populations[2];
double H2_total;

void H2_LevelPops()
{
  double sum_pop = 0.;
  long nEner = 0;
  while( nEner < 3 && sum_pop/H2_total < 0.99999 )
    {
      long ip = H2_ipX_ener_sort[nEner];
      sum_pop += H2_old_populations[ip];
      ++nEner;
    }
}
