/* PR optimization/10157 */
/* Originator: Peter van Hoof <p.van-hoof@qub.ac.uk> */
/* { dg-do compile { target sparc*-*-* } } */
/* { dg-options "-O2 -ffast-math" } */

/* Verify that the loop optimizer doesn't
   emit invalid reg-to-reg copy insns. */

void g() {
  while(1) {
    int i,n;
    double p,r;
    for( i=0; i < n; i++ )
      if( p > 1. )
        for( i=0; i < n; i++ )
          r += 2.;
  }
}
