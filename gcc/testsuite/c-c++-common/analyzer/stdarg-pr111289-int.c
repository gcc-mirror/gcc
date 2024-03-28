#include <stdarg.h>
#include <stdint.h>

typedef unsigned int mode_t;

extern void openat (int, const char *, int, mode_t);

/* Signed vs unsigned of same integral type.  */

static void
test_1 (char const *name, ...)
{
  va_list arg;
  va_start (arg, name);

  mode_t mode = va_arg (arg, mode_t); /* { dg-bogus "-Wanalyzer-va-arg-type-mismatch" } */

  va_end (arg);
  openat (-42, name, 0, mode);
}

void
call_test_1 ()
{
  test_1 ("nonexist.ent/", 0600);
}

/* Not the same size: small enough for int promotion.  */

int16_t global_2;

static void
test_2 (char const *name, ...)
{
  va_list arg;
  va_start (arg, name);

  global_2 = va_arg (arg, int16_t); /* { dg-warning "promoted to 'int'" } */

  va_end (arg);
}

void
call_test_2 ()
{
  test_2 ("nonexist.ent/", 42);
}

/* Not the same size: too big for int promotion.  */

long long global_3;

static void
test_3 (char const *name, ...)
{
  va_list arg;
  va_start (arg, name);

  global_3 = va_arg (arg, long long); /* { dg-warning "'va_arg' expected 'long long int' but received 'int' for variadic argument 1 of 'arg'" } */

  va_end (arg);
}

void
call_test_3 ()
{
  test_3 ("nonexist.ent/", 42);
}
