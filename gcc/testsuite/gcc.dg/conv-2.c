/* { dg-do compile } */
/* { dg-options "-Wpointer-sign" } */

void f1(long *); /* { dg-message "note: expected '\[^\n'\]*' but argument is of type '\[^\n'\]*'" "note: expected" } */
void f2(unsigned long *); /* { dg-message "note: expected '\[^\n'\]*' but argument is of type '\[^\n'\]*'" "note: expected" } */

int main()
{
  long *lp;
  unsigned long *ulp;
  char *cp;
  unsigned char *ucp;
  signed char *scp;

  ulp = lp;	/* { dg-warning " pointer targets in assignment from 'long int \\*' to 'long unsigned int \\*' differ in signedness" } */
  lp = ulp;	/* { dg-warning " pointer targets in assignment from 'long unsigned int \\*' to 'long int \\*' differ in signedness" } */
  f1(ulp);	/* { dg-warning " differ in signedness" } */
  f2(lp);	/* { dg-warning " differ in signedness" } */

  cp = ucp;	/* { dg-warning " pointer targets in assignment from 'unsigned char \\*' to 'char \\*' differ in signedness" } */
  cp = scp;	/* { dg-warning " pointer targets in assignment from 'signed char \\*' to 'char \\*' differ in signedness" } */
  ucp = scp;	/* { dg-warning " pointer targets in assignment from 'signed char \\*' to 'unsigned char \\*' differ in signedness" } */
  ucp = cp;	/* { dg-warning " pointer targets in assignment from 'char \\*' to 'unsigned char \\*' differ in signedness" } */
  scp = ucp;	/* { dg-warning " pointer targets in assignment from 'unsigned char \\*' to 'signed char \\*' differ in signedness" } */
  scp = cp;	/* { dg-warning " pointer targets in assignment from 'char \\*' to 'signed char \\*' differ in signedness" } */
}
