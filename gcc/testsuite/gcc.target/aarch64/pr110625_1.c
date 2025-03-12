/* { dg-do compile } */
/* { dg-options "-Ofast -mcpu=neoverse-n2 -fdump-tree-vect-details -fno-tree-slp-vectorize" } */
/* { dg-final { scan-tree-dump {Vector issue estimate:(?:(?!Cost model).)*reduction latency = 2\n} "vect" } } */

/* Do not increase the vector body cost due to the incorrect reduction latency
    Original vector body cost = 51
    Scalar issue estimate:
      ...
      reduction latency = 2
      estimated min cycles per iteration = 2.000000
      estimated cycles per vector iteration (for VF 2) = 4.000000
    Vector issue estimate:
      ...
      reduction latency = 8      <-- Too large
      estimated min cycles per iteration = 8.000000
    Increasing body cost to 102 because scalar code would issue more quickly
      ...
    missed:  cost model: the vector iteration cost = 102 divided by the scalar iteration cost = 44 is greater or equal to the vectorization factor = 2.
    missed:  not vectorized: vectorization not profitable.  */

typedef struct
{
  unsigned short m1, m2, m3, m4;
} the_struct_t;
typedef struct
{
  double m1, m2, m3, m4, m5;
} the_struct2_t;

double
bar (the_struct2_t *);

double
foo (double *k, unsigned int n, the_struct_t *the_struct)
{
  unsigned int u;
  the_struct2_t result;
  for (u = 0; u < n; u++, k--)
    {
      result.m1 += (*k) * the_struct[u].m1;
      result.m2 += (*k) * the_struct[u].m2;
      result.m3 += (*k) * the_struct[u].m3;
      result.m4 += (*k) * the_struct[u].m4;
    }
  return bar (&result);
}
