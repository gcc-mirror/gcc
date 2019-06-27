int x = 6, w = 8;
int bar (int);

void
foo ()
{
  int y = 5, i;
  #pragma omp teams num_teams(1) firstprivate (x) shared (y) shared (w)
  {
    int z = 7;
    #pragma omp parallel for firstprivate (x, y, z, w) lastprivate (conditional: x, y, z, w)
    for (i = 0; i < 64; i++)
      if (bar (i))
	{
	  x = i;
	  y = i + 1;
	  z = i + 2;
	  w = i + 3;
	}
    bar (y);
    bar (z);
  }
}
