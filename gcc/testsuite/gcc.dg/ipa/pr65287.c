/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

const int __new_sys_siglist[] = {};

extern __typeof(__new_sys_siglist) _new_sys_siglist
    __attribute__((alias("__new_sys_siglist")));
extern __typeof(__new_sys_siglist) _sys_siglist
    __attribute__((alias("__new_sys_siglist")));

int main()
{
  return 0;
}
