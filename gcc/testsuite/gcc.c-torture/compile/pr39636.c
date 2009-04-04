typedef float real;
typedef real rvec[3];
void calc_dx2dx(real *, real *);
void phi_sr(int nj,rvec x[],int k)
{
  int i,j;
  for(i=0; (i<nj-1); i++)
    for(j=i+1; (j<nj); j++)
      if (k)
	calc_dx2dx(x[i],x[j]);
}
