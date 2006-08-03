/* { dg-do run } */
/* { dg-shouldfail "comment" { *-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    abort ();  /* We expect nonzero exit, so this passes.  */
}
