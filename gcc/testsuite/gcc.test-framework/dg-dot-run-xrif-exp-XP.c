/* { dg-do run { xfail empty-*-* } } */
/* { dg-xfail-run-if "" { *-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    return 0;	/* First xfail list didn't match but second did, so XPASS.  */
}
