/* Test typedef redeclaration in C1X.  Side effects from duplicate
   declarations still apply.  */
/* { dg-do run } */
/* { dg-options "-std=c1x -pedantic-errors" } */

extern void exit (int);
extern void abort (void);

int
main (void)
{
  int a = 1, b = 1;
  typedef int T[++a]; /* { dg-message "previous declaration" } */
  typedef int T[++b]; /* { dg-warning "may be a constraint violation at runtime" } */
  if (a != 2 || b != 2)
    abort ();
  exit (0);
}
