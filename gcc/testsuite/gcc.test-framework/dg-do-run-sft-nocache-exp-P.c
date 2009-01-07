/* { dg-do run } */
/* { dg-options "-DDEFINED" } */
/* { dg-shouldfail "comment" { def_nocache } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    abort ();  /* We expect nonzero exit, so this passes.  */
}
