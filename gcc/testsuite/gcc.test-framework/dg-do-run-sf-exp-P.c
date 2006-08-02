/* { dg-do run } */
/* { dg-shouldfail "required comment" } */

extern void abort (void);

int
main ()
{
    abort ();  /* We expect nonzero, so this passes.  */
}
