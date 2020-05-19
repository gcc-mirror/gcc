/* { dg-do compile } */
/* { dg-require-effective-target size32plus } */
/* { dg-additional-options "-march=core-avx2" { target { x86_64-*-* i?86-*-* } } } */
/* { dg-require-effective-target size32plus } */

typedef enum {
 C = 0,               N, S, E, W, T, B,               NE, NW, SE, SW,               NT, NB, ST, SB,               ET, EB, WT, WB,               FLAGS, N_CELL_ENTRIES} CELL_ENTRIES;
typedef double LBM_Grid[(130)*100*100*N_CELL_ENTRIES];
void foo( LBM_Grid srcGrid )
{
  double ux , uy , uz , rho ,         ux1, uy1, uz1, rho1,         ux2, uy2, uz2, rho2,         u2, px, py;
  int i;
  for( i = 0;
       i < (N_CELL_ENTRIES*( 100*100));
       i += N_CELL_ENTRIES )
    {
      rho1 = + ((srcGrid)[((C)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((N)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((S)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((E)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((W)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((T)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((B)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((NE)+N_CELL_ENTRIES*( 100*100))+(i)]) 
	  + ((srcGrid)[((NW)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((SE)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((SW)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((NT)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((NB)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((ST)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((SB)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((ET)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((EB)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((WT)+N_CELL_ENTRIES*( 100*100))+(i)])
	  + ((srcGrid)[((WB)+N_CELL_ENTRIES*( 100*100))+(i)]);
      rho = 2.0*rho1 - rho2;
      px = (((i / N_CELL_ENTRIES) % 100) / (0.5*(100-1))) - 1.0;
      uz = 0.01 * (1.0-px*px) * (1.0-py*py);
      u2 = 1.5 * (ux*ux + uy*uy + uz*uz);
      (((srcGrid)[((C))+(i)])) = (1.0/ 3.0)*rho*(1.0 - u2);
      (((srcGrid)[((N))+(i)])) = (1.0/18.0)*rho*(1.0 + uy*(4.5*uy + 3.0) - u2);
    }
}
