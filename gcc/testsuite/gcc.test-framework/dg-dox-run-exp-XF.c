/* Test the tester; previously gcc.misc-tests/dg-7.c.  */
/* { dg-prms-id 42 } */
/* { dg-do run { xfail *-*-* } } */
extern void abort (void);

main () { abort (); return 0; }
