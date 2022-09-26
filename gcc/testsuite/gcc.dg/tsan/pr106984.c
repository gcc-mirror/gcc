/* { dg-do compile } */
/* { dg-options "-fsanitize=thread" } */

int svcsw (int *ptr, int oldval, int newval)
{
  return __sync_val_compare_and_swap (ptr, oldval, newval);
}
