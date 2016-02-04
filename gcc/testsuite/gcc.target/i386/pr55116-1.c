/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mx32 -maddress-mode=long" } */

int glob_int_arr[100];
int glob_int = 4;

void
expr_global (void)
{
  __builtin_prefetch (glob_int_arr + glob_int, 0, 0);
}
