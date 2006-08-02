/* { dg-do run } */
/* { dg-shouldfail "comment" { unknown-*-* } { "*" } { "" } } */

extern void abort (void);

int
main ()
{
    abort ();  /* Directive is ignored so we expect zero; this fails.  */
}
