typedef struct {
  double x, y;
} vector_t;
double sqrt();
f(int count,vector_t*pos,double r,double *rho)
{
  int i, j, miny, maxy, hy;
  float help, d;
  int gitt[150][150];
  int *data = (int *)malloc(count*sizeof(int));
  for (i = 0; i < count; i++)
    rho[i] = 0;
  for (i = 1; i < count; i++)
    for (hy = miny; hy<= maxy; hy++)
      while(j >=0) {
	d = pos[i].y - pos[j].y;
	if ( d <= r) {
	  d = sqrt(d);
	  rho[i] += help;
	}
      }
}

