/* { dg-do compile } */ 
/* { dg-options "-O2 -fisolate-erroneous-paths-attribute -fdump-tree-isolate-paths -fdump-tree-phicprop1" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */


int z;
int y;

int * foo(int a) __attribute__((returns_nonnull));
int * bar(void) __attribute__((returns_nonnull));

int *
foo(int a)

{
  switch (a)
    {
      case 0:
        return &z;
      default:
        return (int *)0;
    }
}


int *
bar (void)
{
  return 0;
}

/* We testing that the path isolation code can take advantage of the
   returns non-null attribute to isolate a path where NULL flows into
   a return statement.  We test this twice, once where the NULL flows
   from a PHI, the second with an explicit return 0 in the IL.

   We also verify that after isolation phi-cprop simplifies the
   return statement so that it returns &z directly.
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "return &z;" 1 "phicprop1"} } */
/* { dg-final { cleanup-tree-dump "isolate-paths" } } */
/* { dg-final { cleanup-tree-dump "phicprop1" } } */


