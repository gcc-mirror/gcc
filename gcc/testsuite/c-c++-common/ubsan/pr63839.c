/* { dg-do run } */
/* { dg-options "-fsanitize=unreachable" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-shouldfail "ubsan" } */

static void __attribute__ ((noreturn))
bar ()
{
} /* { dg-warning "function does return" } */

void
foo ()
{
  bar ();
}

int
main (void)
{
  foo ();
}

/* { dg-output "execution reached a __builtin_unreachable\\(\\) call" } */
