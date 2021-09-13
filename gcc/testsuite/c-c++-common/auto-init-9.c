/* Verify the variable attribute "uninitialized".  */ 
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */

extern void bar (char, long long *) __attribute__ ((uninitialized)); /* { dg-warning "'uninitialized' attribute ignored because" "is not a variable" } */
extern int __attribute__ ((uninitialized)) boo1; /* { dg-warning "'uninitialized' attribute ignored because 'boo1' is not a local variable" } */
static int __attribute__ ((uninitialized)) boo2; /* { dg-warning "'uninitialized' attribute ignored because 'boo2' is not a local variable" } */

void foo()
{
  short temp1;
  long long __attribute__ ((uninitialized)) temp2[10];
  static int __attribute__ ((uninitialized)) boo3; /* { dg-warning "'uninitialized' attribute ignored because 'boo3' is not a local variable" } */

  bar (temp1, temp2);
  return;
}

/* { dg-final { scan-tree-dump "temp1 = .DEFERRED_INIT \\(2, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump-not "temp2 = .DEFERRED_INIT \\(8, 2, 0\\)" "gimple" } } */
