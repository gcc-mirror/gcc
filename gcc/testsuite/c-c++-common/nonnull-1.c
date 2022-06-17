/* Test for the bad usage of "nonnull" function attribute parms.  */
/*  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull-compare" } */

#include <stddef.h>
#include <stdlib.h>

void foo(void *bar) __attribute__((nonnull(1)));

void foo(void *bar) { if (!bar) abort(); } /* { dg-warning "'nonnull' argument" "bar compared to NULL" } */

extern int func (char *, char *, char *, char *) __attribute__((nonnull));

int
func (char *cp1, char *cp2, char *cp3, char *cp4)
{
  if (cp1) /* { dg-warning "'nonnull' argument" "cp1 compared to NULL" } */
    return 1;

  if (cp2 == NULL) /* { dg-warning "'nonnull' argument" "cp2 compared to NULL" } */
    return 2;

  if (NULL != cp3) /* { dg-warning "'nonnull' argument" "cp3 compared to NULL" } */
    return 3;

  return cp4 != 0 ? 0 : 1; /* { dg-warning "'nonnull' argument" "cp4 compared to NULL" } */
}

__attribute__((nonnull (1))) int
func2 (char *cp)
{
  return (cp != NULL) ? 1 : 0; /* { dg-warning "'nonnull' argument" "cp compared to NULL" } */
}
