/* Test for the "nonnull" function attribute.  */
/* Origin: Jason Thorpe <thorpej@wasabisystems.com> */
/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

#include <stddef.h>

extern void func1 (char *, char *, int) __attribute__((nonnull));

extern void func2 (char *, char *) __attribute__((nonnull(1)));

extern void func3 (char *, int, char *, int)
  __attribute__((nonnull(1,3)));

extern void func4 (char *, char *) __attribute__((nonnull(1)))
  __attribute__((nonnull(2)));

void
foo (int i1, int i2, int i3, char *cp1, char *cp2, char *cp3)
{
  func1(cp1, cp2, i1);

  func1(NULL, cp2, i1); /* { dg-warning "null" "null with argless nonnull 1" } */
  func1(cp1, NULL, i1); /* { dg-warning "null" "null with argless nonnull 2" } */
  func1(cp1, cp2, 0);

  func2(cp1, NULL);
  func2(NULL, cp1); /* { dg-warning "null" "null with single explicit nonnull" } */

  func3(NULL, i2, cp3, i3); /* { dg-warning "null" "null with explicit nonnull 1" } */
  func3(cp3, i2, NULL, i3); /* { dg-warning "null" "null with explicit nonnull 3" } */

  func1(i1 ? cp1 : NULL, cp2, i3); /* { dg-warning "null" "null with cond expr rhs" } */
  func1(i1 ? NULL : cp1, cp2, i3); /* { dg-warning "null" "null with cond expr lhs" } */
  func1(i1 ? (i2 ? cp1 : NULL) : cp2, cp3, i3); /* { dg-warning "null" "null with nested cond expr" } */

  func4(NULL, cp1); /* { dg-warning "null" "null with multiple attributes 1" } */
  func4(cp1, NULL); /* { dg-warning "null" "null with multiple attributes 2" } */
}
