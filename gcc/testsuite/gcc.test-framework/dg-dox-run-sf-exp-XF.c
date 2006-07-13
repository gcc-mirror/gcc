/* { dg-do run { xfail *-*-* } } */
/* { dg-shouldfail "required comment" } */

int
main ()
{
    return 0;  /* We want nonzero but expect to fail; XFAIL.  */
}
