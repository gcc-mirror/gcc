/* { dg-do run { xfail *-*-* } } */
/* { dg-xfail-run-if "" { empty-*-* } } */

extern void abort (void);

int
main ()
{
    abort ();	/* A failed match doesn't override an existing XFAIL.  */
}
