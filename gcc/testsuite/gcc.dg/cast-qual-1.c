/* Incorrect `cast discards `const'' warnings.  There should be warnings
   in bad_cast and bad_assign; bad_assign gets the correct warning, but
   good_cast may get the warning instead of bad_cast.
   gcc 2.7.2.3 passes, egcs-1.1.2 and egcs-ss-19990428 fail.
   http://www.cygnus.com/ml/egcs-bugs/1998-Aug/0635.html */
/* { dg-do compile } */
/* { dg-options "-Wcast-qual" } */
void
good_cast(const void *bar)
{
  (char *const *)bar; /* { dg-bogus "cast discards" "discarding `const' warning" } */
}

void
bad_cast(const void *bar)
{
  (const char **)bar; /* { dg-warning "cast discards" "discarding `const' warning" } */
}

void
good_assign(const void *bar)
{
  char *const *foo = bar;
}

void
bad_assign(const void *bar)
{
  const char **foo = bar; /* { dg-warning "initialization discards" "discarding `const' warning" } */
}
