/* { dg-do run { xfail *-*-* } } */
/* { dg-shouldfail "required comment" } */

extern void abort (void);

int
main ()
{
    abort ();  /* We want nonzero, but expect to fail; XPASS.  */
}
