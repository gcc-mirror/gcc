/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void *memset (void *, int, __SIZE_TYPE__);
extern __SIZE_TYPE__ strlen (const char *);

int
main (int argc, char **argv)
{
  char x[8] = "abc";
  memset (x, argc, strlen (x));
  return 0;
}
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen" { target { ! { powerpc*-*-darwin* hppa*-*-hpux* ia64-*-hpux* } } } } } */
/* hppa*-*-hpux* has an IMPORT statement for strlen (plus the branch). */
/* *-*-darwin* has something similar. */
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen" { target hppa*-*-hpux* } } } */
/* { dg-final { scan-assembler-not "(?n)bl L_strlen\(.*\n\)+.*bl L_strlen" { target powerpc*-*-darwin* } } } */
/* ia64-*-hpux* has a global statement, a type statement, and the branch. */
/* { dg-final { scan-assembler-not "(?n)strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen\(.*\n\)+.*strlen" { target ia64-*-hpux* } } } */
