/* PR c/70651 */
/* { dg-do compile } */
/* { dg-prune-output "\[^\n\r\]*first argument to .va_arg. not of type .va_list.\[^\n\r\]*" } */

void fn1 ()
{
  char **a = 0;
  __builtin_va_arg (a, char **);
}
