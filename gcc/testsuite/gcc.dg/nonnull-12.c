/* Test for the "nonnull_if_nonzero" function attribute.  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

extern void func1 (char *, char *, int, int)
  __attribute__((nonnull_if_nonzero (1, 3, 4), nonnull_if_nonzero (2, 3, 4)));

extern void func2 (char *, char *, unsigned long, unsigned long)
  __attribute__((nonnull_if_nonzero (1, 3, 4)));

enum E { E0 = 0, E1 = 1, E2 = __INT_MAX__ };
extern void func3 (char *, int, char *, enum E, int, enum E)
  __attribute__((nonnull_if_nonzero (1, 4, 6), nonnull_if_nonzero (3, 2, 5)));

extern void func4 (long, char *, char *, long, long, long)
  __attribute__((nonnull_if_nonzero (2, 1, 5)))
  __attribute__((nonnull_if_nonzero (3, 4, 6)));

void
foo (int i1, int i2, int i3, char *cp1, char *cp2, char *cp3)
{
  func1 (cp1, cp2, i1, i2);
  func1 (cp1, cp2, 0, 0);
  func1 (cp1, cp2, 42, 42);
  func1 (NULL, NULL, 0, 0);
  func1 (NULL, NULL, 0, 42);
  func1 (NULL, NULL, 42, 0);
  func1 (NULL, NULL, i1, i2);

  func1 (NULL, cp2, 42, 42); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  func1 (cp1, NULL, 1, 1); /* { dg-warning "argument 2 null where non-null expected because arguments 3 and 4 are nonzero" } */

  func2 (cp1, NULL, 17, 17);
  func2 (NULL, cp2, 0, 0);
  func2 (NULL, cp2, 0, 17);
  func2 (NULL, cp2, 17, 0);
  func2 (NULL, cp1, 2, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */

  func3 (NULL, i2, cp3, i3, i3, i2);
  func3 (cp1, i2, NULL, i3, i3, i2);
  func3 (NULL, i2, cp3, E0, i3, E0);
  func3 (NULL, i2, cp3, E0, i3, E1);
  func3 (NULL, i2, cp3, E1, i3, E0);
  func3 (cp1, 0, NULL, E2, 0, E2);
  func3 (cp1, 0, NULL, E2, 4, E2);
  func3 (cp1, 4, NULL, E2, 0, E2);
  func3 (NULL, i2, cp3, E2, i3, E2); /* { dg-warning "argument 1 null where non-null expected because arguments 4 and 6 are nonzero" } */
  func3 (cp3, 5, NULL, i3, 1, i2); /* { dg-warning "argument 3 null where non-null expected because arguments 2 and 5 are nonzero" } */

  func1 (i2 ? cp1 : NULL, cp2, i3, i3);
  func1 (i2 ? NULL : cp1, cp2, i3, i3);
  func1 (i2 ? (i3 ? cp1 : NULL) : cp2, cp3, i1, i1);
  func1 (i1 ? cp1 : NULL, cp2, 0, 0);
  func1 (i1 ? cp1 : NULL, cp2, 0, 4);
  func1 (i1 ? cp1 : NULL, cp2, 4, 0);
  func1 (i1 ? NULL : cp1, cp2, 0, 0);
  func1 (i1 ? NULL : cp1, cp2, 0, 2);
  func1 (i1 ? NULL : cp1, cp2, 3, 0);
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 0, 0);
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 0, 1);
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 2, 0);
  func1 (i1 ? cp1 : NULL, cp2, 1, 2); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  func1 (i1 ? NULL : cp1, cp2, 2, 3); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 3, 4); /* { dg-warning "argument 1 null where non-null expected because arguments 3 and 4 are nonzero" } */

  func4 (0, NULL, NULL, 0, 0, 0);
  func4 (0, NULL, NULL, 0, 1, 2);
  func4 (3, NULL, NULL, 4, 0, 0);
  func4 (-1, NULL, cp1, 0, 42, 0); /* { dg-warning "argument 2 null where non-null expected because arguments 1 and 5 are nonzero" } */
  func4 (0, cp1, NULL, 77, 0, 12); /* { dg-warning "argument 3 null where non-null expected because arguments 4 and 6 are nonzero" } */
}
