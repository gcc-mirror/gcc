/* Verify zero initialization for VLA automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-rtl-expand" } */

extern void bar (int);

void foo(int n)
{
  int arr[n];
  bar (arr[2]);
  return;
}

/* { dg-final { scan-rtl-dump "__builtin_memset" "expand" } } */
