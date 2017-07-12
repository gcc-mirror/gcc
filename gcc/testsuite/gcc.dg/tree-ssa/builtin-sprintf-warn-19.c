/* PR tree-optimization/80397 - missing -Wformat-overflow with arguments
   of enum types
   { dg-do compile }
   { dg-options "-O2 -Wall -Wformat-overflow=1 -ftrack-macro-expansion=0" }
   { dg-require-effective-target int32plus } */

void sink (char*);

long long integer_range (long long min, long long max)
{
  extern long long integer_value (void);
  long long n = integer_value ();
  return n < min || max < n ? min : n;
}

typedef enum { i0, imax = __INT_MAX__ } Int;
typedef enum { ll0, llmax = __LONG_LONG_MAX__ } LLong;

#define R(T, min, max) (T)integer_range (min, max)

char buffer[1];
#define T(fmt, ...)						\
  __builtin_sprintf (buffer + 1, fmt, __VA_ARGS__), sink (buffer)

void test_bool (_Bool b)
{
  T ("%hhi", b);   // { dg-warning "writing 1 byte" }
  T ( "%hi", b);   // { dg-warning "writing 1 byte" }
  T (  "%i", b);   // { dg-warning "writing 1 byte" }
}

void test_enum (void)
{
  T ("%hhi", R (Int,    1,     1));   // { dg-warning "writing 1 byte" }
  T ("%hhi", R (Int,    1,    22));   // { dg-warning "between 1 and 2 bytes" }

  T ( "%hi", R (Int,    1,     2));   // { dg-warning "writing 1 " }
  T ( "%hi", R (Int,    1,    22));   // { dg-warning "between 1 and 2 " }
  T ( "%hi", R (Int,   22,   333));   // { dg-warning "between 2 and 3 " }
  T ( "%hi", R (Int,  333,  4444));   // { dg-warning "between 3 and 4 " }

  T (  "%i", R (Int,    1,     1));   // { dg-warning "writing 1 " }
  T (  "%i", R (Int,    1,    22));   // { dg-warning "between 1 and 2 " }
  T (  "%i", R (Int,   22,   333));   // { dg-warning "between 2 and 3 " }
  T (  "%i", R (Int,  333,  4444));   // { dg-warning "between 3 and 4 " }
  T (  "%i", R (Int, 4444, 55555));   // { dg-warning "between 4 and 5 " }

#if __LONG_MAX__ == __LONG_LONG_MAX__
#  define LLI "%li"
#else
#  define LLI "%lli"
#endif

  T (LLI, R (LLong,    1,     1));       // { dg-warning "writing 1 " }
  T (LLI, R (LLong,    1,    22));       // { dg-warning "between 1 and 2 " }
  T (LLI, R (LLong,   22,   333));       // { dg-warning "between 2 and 3 " }
  T (LLI, R (LLong,  333,  4444));       // { dg-warning "between 3 and 4 " }
  T (LLI, R (LLong, 4444, 55555));       // { dg-warning "between 4 and 5 " }

  T (LLI, R (LLong, 4444, 1234567890));  // { dg-warning "between 4 and 10 " }
  T (LLI, R (LLong, 4444, 12345678901)); // { dg-warning "between 4 and 11 " }
}
