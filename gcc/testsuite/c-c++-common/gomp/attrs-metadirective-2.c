/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */

#define N 100

int
main (void)
{
  int x = 0;
  int y = 0;

  /* Test implicit default (nothing).  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: barrier))]]
    x = 1;

  /* Test with multiple standalone directives.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: barrier),
      default (flush))]]
    x = 1;

  /* Test combining a standalone directive with one that takes a statement
     body.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: parallel),
      default (barrier))]]
    x = 1;

  /* Test combining a standalone directive with one that takes a for loop.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: parallel for),
      default (barrier))]]
    for (int i = 0; i < N; i++)
      x += i;

  /* Test combining a directive that takes a for loop with one that takes
     a regular statement body.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: parallel for),
      default (parallel))]]
    for (int i = 0; i < N; i++)
      x += i;

  /* Test labels inside statement body.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: teams num_teams(512)),
      when (device={arch("gcn")}: teams num_teams(256)),
    default (teams num_teams(4)))]]
  {
    if (x)
      goto l1;
    else
      goto l2;
  l1: ;
  l2: ;
  }

  /* Test local labels inside statement body.  */
  [[omp::directive (metadirective,
      when (device={arch("nvptx")}: teams num_teams(512)),
      when (device={arch("gcn")}: teams num_teams(256)),
      default (teams num_teams(4)))]]
  {
    //__label__ l1, l2;

    if (x)
      goto l1;
    else
      goto l2;
  l1: ;
  l2: ;
  }

  return 0;
}
