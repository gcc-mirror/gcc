/* Verify zero initialization for integer and pointer type automatic variables.  */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */

#ifndef __cplusplus
# define bool _Bool
#endif

enum E {
  N1 = 0,
  N2,
  N3
};

extern void bar (char, short, int, enum E, long, long long, int *, bool);

void foo()
{
  char temp1;
  short temp2;
  int temp3;
  enum E temp4;
  long temp5;
  long long temp6;
  int *temp7;
  bool temp8;

  bar (temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8);
  return;
}

/* { dg-final { scan-tree-dump "temp1 = .DEFERRED_INIT \\(1, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp2 = .DEFERRED_INIT \\(2, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp3 = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp4 = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp5 = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" { target ilp32 } } } */
/* { dg-final { scan-tree-dump "temp5 = .DEFERRED_INIT \\(8, 2, 0\\)" "gimple" { target lp64 } } } */
/* { dg-final { scan-tree-dump "temp6 = .DEFERRED_INIT \\(8, 2, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp7 = .DEFERRED_INIT \\(4, 2, 0\\)" "gimple" { target ilp32 } } } */
/* { dg-final { scan-tree-dump "temp7 = .DEFERRED_INIT \\(8, 2, 0\\)" "gimple" { target lp64 } } } */
/* { dg-final { scan-tree-dump "temp8 = .DEFERRED_INIT \\(1, 2, 0\\)" "gimple" } } */
