/* { dg-do run } */
/* { dg-shouldfail "required comment" } */

int
main ()
{
    return 0;  /* We expect nonzero, so this fails.  */
}
