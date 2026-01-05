/* { dg-do compile } */
/* { dg-additional-options "-O3 -mcpu=neoverse-v2 -fdump-tree-vect-all -w" } */

#include <cstddef>
#include <cstdint>

std::ptrdiff_t getRunReps ();

double * get_rp ();
std::ptrdiff_t get_it ();

void
runSeqVariant ()
{
  const std::ptrdiff_t run_reps = getRunReps ();

  double *__restrict__ B = get_rp ();
  double *__restrict__ D = get_rp ();
  double *__restrict__ M = get_rp ();
  std::ptrdiff_t NE = get_it ();

  for (volatile int irep = 0; irep < run_reps; ++irep)
    {
      for (int e = 0; e < NE; ++e)
        {
          double s_B[5][4];

          for (int d = 0; d < 4; d++)
            for (int q = 0; q < 5; q++)
              s_B[q][d] = B[q + 5 * d];

          double s_D[5][5][5];

          for (int k1 = 0; k1 < 5; k1++)
            for (int k2 = 0; k2 < 5; k2++)
              for (int k3 = 0; k3 < 5; k3++)
                s_D[k1][k2][k3] = D[k1 + 5 * k2 + 5 * 5 * k3 + 5 * 5 * 5 * e];

          for (int i1 = 0; i1 < 4; i1++)
            for (int i2 = 0; i2 < 4; i2++)
              for (int i3 = 0; i3 < 4; i3++)
                for (int j1 = 0; j1 < 4; ++j1)
                  for (int j2 = 0; j2 < 4; ++j2)
                    for (int j3 = 0; j3 < 4; ++j3)
                      {
                        double val = 0.0;
                        for (int k1 = 0; k1 < 5; ++k1)
                          for (int k2 = 0; k2 < 5; ++k2)
                            for (int k3 = 0; k3 < 5; ++k3)
                              val += s_B[k1][i1] * s_B[k1][j1] * s_B[k2][i2]
                                     * s_B[k2][j2] * s_B[k3][i3] * s_B[k3][j3]
                                     * s_D[k1][k2][k3];
                        // clang-format off
                        M[i1 + 4 * (i2 + 4 * (i3 + 4 * (j1 + 4 * (j2 + 4  * (j3 + 4 * e)))))] = val;
                        //clang-format on
                      }
        }
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "low throughput of per iteration due to splats" "vect" } } */
