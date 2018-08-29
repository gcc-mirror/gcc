/* { dg-do compile } */
/* { dg-options "-Ofast -floop-nest-optimize" } */

typedef struct _IO_FILE FILE;
extern struct _IO_FILE *stderr;
typedef float real;
typedef real rvec[3];
int rgbset (int);
void ps_box (int, int);
void plot_phi(char *fn,rvec box,int natoms,rvec x[],real phi[])
{
  real phi_max,rr,gg,bb,fac,dx,x0,y0;
  int i;
  for(i=0; (i<natoms); i++) 
    phi_max=((phi_max > __builtin_fabs(phi[i]))
	     ? phi_max : __builtin_fabs(phi[i]));
  if (__builtin_fabs(phi_max)<1.2e-38)
      __builtin_fprintf(stderr, "X");
  ps_box((real)(fac*box[0]-1),(real)(fac*box[1]-1));
  for(i=0; (i<natoms); i++)
    {
      rr=gg=bb=1.0;
      if (phi[i] < 0)
	gg=bb=(1.0+(phi[i]/phi_max));
      else
	rr=gg=(1.0-(phi[i]/phi_max));
      rr=rgbset(rr);
    }
}
