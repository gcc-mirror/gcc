/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-cddce1-details" } */
/* PR tree-optimization/122037 */

void bar1 (char *, int) __attribute__((noreturn));
void foo1 (int size)
{
  char temp[size];
  temp[size-1] = '\0';
  bar1 (temp, size);
}

/* The call to __builtin_stack_save should have been removed. */
/* { dg-final { scan-tree-dump "Deleting : __builtin_stack_save" "cddce1" } } */
/* { dg-final { scan-tree-dump-not "__builtin_stack_save " "optimized" } } */
