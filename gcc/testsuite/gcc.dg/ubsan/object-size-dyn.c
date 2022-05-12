/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=undefined" } */
#include <stdio.h>

int
__attribute__ ((noinline))
dyn (int size, int i)
{
  __builtin_printf ("dyn\n");
  fflush (stdout);
  int *alloc = __builtin_calloc (size, sizeof (int));
  int ret = alloc[i];
  __builtin_free (alloc);
  return ret;
}

int
__attribute__ ((noinline))
off (int size, int i, int ret)
{
  char *mem = __builtin_alloca (size);
  mem += size - 1;

  return (int) mem[i] & ret;
}

int
main (void)
{
  int ret = dyn (2, 2);

  ret |= off (4, 4, 0);

  return ret;
}

/* { dg-output "load of address \[^\n\r]* with insufficient space for an object of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*load of address \[^\n\r]* with insufficient space for an object of type 'char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*note: pointer points here\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*\\^" } */
