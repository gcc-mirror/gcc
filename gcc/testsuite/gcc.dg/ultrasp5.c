/* PR target/10072 */
/* Originator: Peter van Hoof <p.van-hoof@qub.ac.uk> */
/* { dg-do compile { target sparc-*-* } } */
/* { dg-options "-std=c99 -O1 -mcpu=ultrasparc -ffast-math" } */

void p(int v)
{
  int i=v,j;
  float a,b,c,x[i];

  x[i] = (a/(((b)>(c)) ? (b) : (c)) - (((i) == (j)) ? 1.f : 0.f));
}
