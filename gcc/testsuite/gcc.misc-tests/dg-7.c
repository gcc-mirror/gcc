/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-do run { xfail *-*-* } } */
extern void abort (void);

main () { abort (); return 0; }
