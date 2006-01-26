/* { dg-do compile } */
/* { dg-options "-Wpointer-sign" } */

void f1(long *);
void f2(unsigned long *);

int main()
{
  long *lp;
  unsigned long *ulp;
  char *cp;
  unsigned char *ucp;
  signed char *scp;

  ulp = lp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  lp = ulp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  f1(ulp);	/* { dg-warning " differ in signedness" } */
  f2(lp);	/* { dg-warning " differ in signedness" } */

  cp = ucp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  cp = scp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  ucp = scp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  ucp = cp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  scp = ucp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
  scp = cp;	/* { dg-warning " pointer targets in assignment differ in signedness" } */
}
