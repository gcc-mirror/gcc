/* { dg-do run } */
/* { dg-xfail-run-if "" { *-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    abort ();	/* This results in an expected failure.  */
}
