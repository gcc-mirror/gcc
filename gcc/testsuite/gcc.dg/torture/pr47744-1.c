/* { dg-do compile } */
/* { dg-options "-fpic" { target fpic } } */

typedef int int32_t;
static const int init_jk[] = {2,3,4,6};
 int __kernel_rem_pio2(double *x, double *y, int e0, int nx, int prec, const int32_t *ipio2)
{
 int32_t jz,jx,jv,jp,jk,carry,n,iq[20],i,j,k,m,q0,ih;
 double z,fw,f[20],fq[20],q[20];
 jk = init_jk[prec];
 jp = jk;
 jx = nx-1;
 for (i=0;i<=jk;i++) {
     for(j=0,fw=0.0;j<=jx;j++) fw += x[j]*f[jx+i-j]; q[i] = fw;
 }
 for(i=0,j=jz,z=q[jz];j>0;i++,j--) {
     z = q[j-1]+fw;
 }
 n = (int32_t) z;
 return n&7;
}
