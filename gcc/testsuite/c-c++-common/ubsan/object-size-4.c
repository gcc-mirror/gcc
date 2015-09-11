/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=object-size" } */

/* Test that we instrument flexible array members.  */

struct T { int l; int a[]; };
struct U { int l; int a[0]; };

int
main (void)
{
  volatile int i;
  struct T *t = (struct T *) __builtin_calloc (sizeof (struct T)
					       + sizeof (int), 1);
  i = t->a[1];

  struct U *u = (struct U *) __builtin_calloc (sizeof (struct U)
					       + sizeof (int), 1);
  i = u->a[1];
  return 0;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */
