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
  char *const *foo = bar; /* { dg-bogus "initialization discards" "discarding `const' warning" } */
}

void
bad_assign(const void *bar)
{
  const char **foo = bar; /* { dg-warning "initialization discards" "discarding `const' warning" } */
}

typedef struct rtx_def * rtx;

void
typedef_cast(const void *bar)
{
  (const rtx)bar; /* { dg-bogus "cast discards" "discarding `const' warning" } */
  (const rtx *)bar; /* { dg-warning "cast discards" "discarding `const' warning" } */
}

void
typedef_cast2(const rtx bar)
{
  (const void *)bar; /* { dg-bogus "cast discards" "discarding `const' warning" } */
  (const void **)bar; /* { dg-warning "cast discards" "discarding `const' warning" } */
}

void
typedef_assign(const void *bar)
{
  rtx const *foo1 = bar; /* { dg-bogus "initialization discards" "discarding `const' warning" } */
  const rtx *foo2 = bar; /* { dg-warning "initialization discards" "discarding `const' warning" } */
}

void
typdef_assign2(const rtx bar)
{
  void *const *foo1 = bar; /* { dg-bogus "initialization discards" "discarding `const' warning" } */
  const void **foo2 = bar; /* { dg-warning "initialization discards" "discarding `const' warning" } */
}
