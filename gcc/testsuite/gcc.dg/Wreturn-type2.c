/* Test for cases that should not get the unconditional warning about
   missing return.  */
/* { dg-do compile } */

extern void abort (void);

f() {}				/* { dg-bogus "" "no return warning" } */
int g() { abort (); }		/* { dg-bogus "" "no return warning" } */
int main() {}			/* { dg-bogus "" "no return warning" } */
