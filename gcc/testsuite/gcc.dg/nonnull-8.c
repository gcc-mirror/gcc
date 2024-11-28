/* Test for the "nonnull_if_nonzero" function attribute.  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

extern void func1 (char *, char *, int)
  __attribute__((nonnull_if_nonzero (1, 3), nonnull_if_nonzero (2, 3)));

extern void func2 (char *, char *, unsigned long)
  __attribute__((nonnull_if_nonzero (1, 3)));

enum E { E0 = 0, E1 = __INT_MAX__ };
extern void func3 (char *, int, char *, enum E)
  __attribute__((nonnull_if_nonzero (1, 4), nonnull_if_nonzero (3, 2)));

extern void func4 (long, char *, char *, long)
  __attribute__((nonnull_if_nonzero (2, 1)))
  __attribute__((nonnull_if_nonzero (3, 4)));

void
foo (int i1, int i2, int i3, char *cp1, char *cp2, char *cp3)
{
  func1 (cp1, cp2, i1);
  func1 (cp1, cp2, 0);
  func1 (cp1, cp2, 42);
  func1 (NULL, NULL, 0);
  func1 (NULL, NULL, i1);

  func1 (NULL, cp2, 42); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  func1 (cp1, NULL, 1); /* { dg-warning "argument 2 null where non-null expected because argument 3 is nonzero" } */

  func2 (cp1, NULL, 17);
  func2 (NULL, cp2, 0);
  func2 (NULL, cp1, 2); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */

  func3 (NULL, i2, cp3, i3);
  func3 (cp1, i2, NULL, i3);
  func3 (NULL, i2, cp3, E0);
  func3 (cp1, 0, NULL, E1);
  func3 (NULL, i2, cp3, E1); /* { dg-warning "argument 1 null where non-null expected because argument 4 is nonzero" } */
  func3 (cp3, 5, NULL, i3); /* { dg-warning "argument 3 null where non-null expected because argument 2 is nonzero" } */

  func1 (i2 ? cp1 : NULL, cp2, i3);
  func1 (i2 ? NULL : cp1, cp2, i3);
  func1 (i2 ? (i3 ? cp1 : NULL) : cp2, cp3, i1);
  func1 (i1 ? cp1 : NULL, cp2, 0);
  func1 (i1 ? NULL : cp1, cp2, 0);
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 0);
  func1 (i1 ? cp1 : NULL, cp2, 1); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  func1 (i1 ? NULL : cp1, cp2, 2); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */
  func1 (i1 ? (i2 ? cp1 : NULL) : cp2, cp3, 3); /* { dg-warning "argument 1 null where non-null expected because argument 3 is nonzero" } */

  func4 (0, NULL, NULL, 0);
  func4 (-1, NULL, cp1, 0); /* { dg-warning "argument 2 null where non-null expected because argument 1 is nonzero" } */
  func4 (0, cp1, NULL, 77); /* { dg-warning "argument 3 null where non-null expected because argument 4 is nonzero" } */
}
