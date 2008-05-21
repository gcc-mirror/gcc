/* { dg-do run { xfail *-empty-* } } */
/* { dg-xfail-run-if "" { empty-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    return 0;	/* Neither xfail list matched, so pass.  */
}
