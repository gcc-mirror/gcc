/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O3 -ftree-parallelize-loops=4" } */

struct p7prior_s {
  int   mnum;        /* number of mat emission Dirichlet mixtures */
  float mq[200];     /* probabilities of mnum components          */
  float m[200][20];  /* match emission terms per mix component    */
};

struct p7prior_s *default_amino_prior(void);
struct p7prior_s *P7AllocPrior(void);

struct p7prior_s *
default_amino_prior(void)
{
  struct p7prior_s *pri;
  int x, q;

  static float defmq[5] = {
    0.178091, 0.056591, 0.0960191, 0.0781233, 0.0834977 };
 static float defm[5][6] = {
    { 0.270671, 0.039848, 0.017576, 0.016415, 0.014268, 0.216147 },
    { 0.021465, 0.010300, 0.011741, 0.010883, 0.385651, 0.029156 },
    { 0.561459, 0.045448, 0.438366, 0.764167, 0.087364, 0.583402 },
    { 0.070143, 0.011140, 0.019479, 0.094657, 0.013162, 0.073732 },
    { 0.041103, 0.014794, 0.005610, 0.010216, 0.153602, 0.012049 }
  };

  pri = P7AllocPrior();
  pri->mnum  = 5;
  for (q = 0; q < pri->mnum; q++)
    {
      pri->mq[q] = defmq[q];
      for (x = 0; x < 6; x++)
        pri->m[q][x] = defm[q][x];
    }
  return pri;
}

