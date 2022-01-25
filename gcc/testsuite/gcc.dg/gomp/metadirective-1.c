int main (void)
{
  int x, y;

  /* Test nested functions inside statement body.  */
  #pragma omp metadirective \
    when (device={arch("nvptx")}: teams num_teams(512)) \
    when (device={arch("gcn")}: teams num_teams(256)) \
    default (teams num_teams(4))
  {
    int f (int x) { return x * 3; }

    y = f (x);
  }
}
