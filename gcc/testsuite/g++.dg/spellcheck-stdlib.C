/* Missing <cstddef>.  */

void *ptr = NULL; // { dg-error "'NULL' was not declared" }
// { dg-message "'NULL' is defined in header '<cstddef>'; this is probably fixable by adding '#include <cstddef>'" "" { target *-*-* } .-1 }

ptrdiff_t pd; // { dg-error "'ptrdiff_t' does not name a type" }
// { dg-message "'ptrdiff_t' is defined in header '<cstddef>'; this is probably fixable by adding '#include <cstddef>'" "" { target *-*-* } .-1 }

size_t sz; // { dg-error "'size_t' does not name a type" }
// { dg-message "'size_t' is defined in header '<cstddef>'; this is probably fixable by adding '#include <cstddef>'" "" { target *-*-* } .-1 }

/* Missing <cstdio>.  */

void test_cstdio (void)
{
  FILE *f; // { dg-error "'FILE' was not declared in this scope" }
  // { dg-message "'FILE' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }
  // { dg-error "'f' was not declared in this scope" "" { target *-*-* } .-2 }
  // { dg-bogus "suggested alternative: 'if'" "PR c++/80567" { target *-*-* } .-3 }

  char buf[BUFSIZ]; // { dg-error "'BUFSIZ' was not declared" }
  // { dg-message "'BUFSIZ' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  char buf2[FILENAME_MAX]; // { dg-error "'FILENAME_MAX' was not declared" }
  // { dg-message "'FILENAME_MAX' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  stderr; // { dg-error "'stderr' was not declared" }
  // { dg-message "'stderr' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  stdin; // { dg-error "'stdin' was not declared" }
  // { dg-message "'stdin' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  stdout; // { dg-error "'stdout' was not declared" }
  // { dg-message "'stdout' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  EOF; // { dg-error "'EOF' was not declared" }
  // { dg-message "'EOF' is defined in header '<cstdio>'; this is probably fixable by adding '#include <cstdio>'" "" { target *-*-* } .-1 }

  fopen ("test.txt"); // { dg-error "'fopen' was not declared" }
  // { dg-message "'#include <cstdio>'" "" { target *-*-* } .-1 }

  printf ("test\n"); // { dg-error "'printf' was not declared" }
  // { dg-message "'#include <cstdio>'" "" { target *-*-* } .-1 }
  
  char tmp[16];
  sprintf (tmp, "test\n");  // { dg-error "'sprintf' was not declared" }
  // { dg-message "'#include <cstdio>'" "" { target *-*-* } .-1 }
  snprintf (tmp, 16, "test\n");  // { dg-error "'snprintf' was not declared" }
  // { dg-message "'#include <cstdio>'" "" { target *-*-* } .-1 }

  getchar ();  // { dg-error "'getchar' was not declared" }
  // { dg-message "'#include <cstdio>'" "" { target *-*-* } .-1 }
}

/* Missing <cerrno>.  */

int test_cerrno (void)
{
  return errno; // { dg-error "'errno' was not declared" }
  // { dg-message "'errno' is defined in header '<cerrno>'; this is probably fixable by adding '#include <cerrno>'" "" { target *-*-* } .-1 }
}

/* Missing <cstdarg>.  */

void test_cstdarg (void)
{
  va_list ap; // { dg-error "'va_list'" }
  // { dg-message "'va_list' is defined in header '<cstdarg>'; this is probably fixable by adding '#include <cstdarg>'" "" { target *-*-* } .-1 }
}

/* Missing <climits>.  */
int test_INT_MAX (void)
{
  return INT_MAX; // { dg-line INT_MAX_line }
  // { dg-error "'INT_MAX' was not declared" "" { target *-*-* } INT_MAX_line }
  // { dg-bogus "__INT_MAX__" "" { target *-*-* } INT_MAX_line }
  // { dg-message "'INT_MAX' is defined in header '<climits>'; this is probably fixable by adding '#include <climits>'" "" { target *-*-* } INT_MAX_line }
}

/* Missing <cfloat>.  */
float test_FLT_MAX = FLT_MAX; // { dg-line FLT_MAX_line }
// { dg-error "'FLT_MAX' was not declared" "" { target *-*-* } FLT_MAX_line }
// { dg-message "'FLT_MAX' is defined in header '<cfloat>'; this is probably fixable by adding '#include <cfloat>'" "" { target *-*-* } FLT_MAX_line }

/* Missing <cstring>.  */

void test_cstring (char *dest, char *src)
{
  memchr(dest, 'a', 4); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  memcmp(dest, src, 4); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  memcpy(dest, src, 4); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  memmove(dest, src, 4); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  memset(dest, 'a', 4); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strcat(dest, "test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strchr("test", 'e'); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strcmp(dest, "test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strcpy(dest, "test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strlen("test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strncat(dest, "test", 3); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strncmp(dest, "test", 3); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strncpy(dest, "test", 3); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strrchr("test", 'e'); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strspn(dest, "test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
  strstr(dest, "test"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstring>'" "" { target *-*-* } .-1 }
}

/* Missing <cassert>.  */

void test_cassert (int a, int b)
{
  assert (a == b); // { dg-error "was not declared" }
  // { dg-message "'#include <cassert>'" "" { target *-*-* } .-1 }
}

/* Missing <cstdlib>.  */

void test_cstdlib (void *q)
{
  void *ptr = malloc (64); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  free (ptr); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  q = realloc (q, 1024); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  q = calloc (8, 8); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }

  void callback ();
  atexit (callback); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  int i;
  i = EXIT_SUCCESS; // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  i = EXIT_FAILURE; // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  exit (i); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
  abort (); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }

  getenv ("foo"); // { dg-error "was not declared" }
  // { dg-message "'#include <cstdlib>'" "" { target *-*-* } .-1 }
}

/* Missing <ctime>.  */

void test_ctime (void *q, long s, double d)
{
  clock_t c; // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  time_t t; // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  tm t2; // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  d = difftime (0, 0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  s = mktime (q); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  s = time (0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  q = asctime (0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  q = ctime (0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  q = gmtime (0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  q = localtime (0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
  char c[2];
  strftime (c, 2, "", 0); // { dg-error "was not declared" }
  // { dg-message "'#include <ctime>'" "" { target *-*-* } .-1 }
}

/* Verify that we don't offer suggestions to stdlib globals names when
   there's an explicit namespace.  */

namespace some_ns {}

int not_within_namespace (void)
{
  return some_ns::stdout; // { dg-error "'stdout' is not a member of 'some_ns'" }
  // { dg-bogus "is defined in header" "" { target *-*-* } .-1 }
}

/* Similarly for when there's an explicit class scope.  */

class some_class {};

int not_within_class (void)
{
  return some_class::stdout; // { dg-error "'stdout' is not a member of 'some_class'" }
  // { dg-bogus "is defined in header" "" { target *-*-* } .-1 }
}
