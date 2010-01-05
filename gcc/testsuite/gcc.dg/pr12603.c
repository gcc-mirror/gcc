/* PR 12603: No return statement warning on function that never returns with -O3. */
/* { dg-do compile } */
/* { dg-options "-O3 -Wall -Wextra -Wreturn-type" } */
int
this_function_never_returns ()
{
  for (;;);
}
