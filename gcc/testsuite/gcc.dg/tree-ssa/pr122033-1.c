/* PR middle-end/122033 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar1 (char *, int);
void bar3(void) __attribute__((noreturn));
void foo1 (int size)
{
  {
    char temp[size];
    temp[size-1] = '\0';
    bar1 (temp, size);
  }
  bar3 ();
}

/* { dg-final { scan-tree-dump-not "__builtin_stack_save" "optimized"} } */
/* { dg-final { scan-tree-dump-not "__builtin_stack_restore" "optimized"} } */
