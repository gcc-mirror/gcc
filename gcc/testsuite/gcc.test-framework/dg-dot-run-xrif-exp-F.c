/* { dg-do run { xfail *-empty-* } } */
/* { dg-xfail-run-if "" { empty-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    abort ();	/* Neither xfail list matched, so fail.  */
}
