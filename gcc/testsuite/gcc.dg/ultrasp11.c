/* PR target/17245 */
/* Origin: <aaronw@net.com> */
/* Testcase by Christian Ehrhardt <ehrhardt@mathematik.uni-ulm.de> */
/* { dg-do compile { target sparc*-*-* } } */
/* { dg-options "-O -mcpu=v9" } */

/* This used to fail on 32-bit Ultrasparc because reload was emitting
   a move insn that doesn't satisfy its constraints.  */

int n; 
double range ;
double bin ;
double wmean;

double f ()
{
  int i ;
  long double W = 0 ;
  for ( i = 0 ; i < n ; i ++) {
    double xi = range;
    double wi = bin;
    W += wi ;
    wmean += ( xi - wmean) * ( wi / W);
  }
}
