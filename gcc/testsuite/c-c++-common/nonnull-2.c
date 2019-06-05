/* Test for the bad usage of "nonnull" function attribute parms.  */
/* { dg-do compile } */
/* { dg-options "-Wnonnull-compare" } */

void bar (char **);

__attribute__((nonnull (1, 3))) int
foo (char *cp1, char *cp2, char *cp3, char *cp4)
{
  if (cp1 == (char *) 0) /* { dg-warning "'nonnull' argument" "cp1 compared to NULL" } */
    return 1;

  cp1 = cp2;
  if (cp1 == (char *) 0) /* { dg-bogus "'nonnull' argument" } */
    return 2;

  if (!cp4)	   /* { dg-bogus "'nonnull' argument" } */
    return 3;

  char **p = &cp3;
  bar (p);
  if (cp3 == (char *) 0) /* { dg-bogus "'nonnull' argument" } */
    return 4;

  return 5;
}
