/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions -fdump-tree-optimized" } */

void af (void *a);

void
bf (void)
{
  int i = 1;
  char v[i];
  af (v);
}

/* { dg-final { scan-tree-dump-not "__builtin_stack_save" "optimized"} } */
/* { dg-final { scan-tree-dump-not "__builtin_stack_restore" "optimized"} } */
