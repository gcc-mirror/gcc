/* { dg-do run } */
/* { dg-xfail-run-if "" { empty-*-* } } */

extern void abort (void);

int
main ()
{
    abort ();	/* This results in a failure.  */
}
