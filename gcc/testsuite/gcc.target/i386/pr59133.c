/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -march=core-avx2" } */

#define XX 0
#define YY 1
#define ZZ 2
#define DIM 3
typedef float matrix[DIM][DIM];
typedef float rvec[DIM];
extern int det (matrix);
extern void foo(matrix);

void bar1 (int n,int *index,rvec x[],matrix trans)
{
  float   xt,yt,zt;
  int    i,ii;
  
  for(i=0; (i<n); i++) {
    ii=index ? index[i] : i;
    xt=x[ii][XX];
    yt=x[ii][YY];
    zt=x[ii][ZZ];
    x[ii][XX]=trans[XX][XX]*xt+trans[XX][YY]*yt+trans[XX][ZZ]*zt;
    x[ii][YY]=trans[YY][XX]*xt+trans[YY][YY]*yt+trans[YY][ZZ]*zt;
    x[ii][ZZ]=trans[ZZ][XX]*xt+trans[ZZ][YY]*yt+trans[ZZ][ZZ]*zt;
  }
}


void bar2 (int n, rvec x[]) 
{
  int     m;
  matrix  trans;
  
  foo (trans);
  
  if (det (trans) < 0) {
    for(m=0; (m<DIM); m++)
      trans[ZZ][m] = -trans[ZZ][m];
  }  
  bar1 (n,(int*) 0,x,trans);
}
