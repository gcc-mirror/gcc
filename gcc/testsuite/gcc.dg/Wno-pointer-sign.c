/* { dg-do compile } */
/* { dg-options "-Wno-pointer-sign" } */

void f1(long *);
void f2(unsigned long *);

int main()
{
  long *lp;
  unsigned long *ulp;
  char *cp;
  unsigned char *ucp;
  signed char *scp;

  ulp = lp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  lp = ulp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  f1(ulp);	/* { dg-bogus " differ in signedness" } */
  f2(lp);	/* { dg-bogus " differ in signedness" } */

  cp = ucp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  cp = scp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  ucp = scp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  ucp = cp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  scp = ucp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
  scp = cp;	/* { dg-bogus " pointer targets in assignment differ in signedness" } */
}
