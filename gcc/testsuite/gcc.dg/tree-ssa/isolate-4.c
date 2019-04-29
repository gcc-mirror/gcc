/* { dg-do compile } */ 
/* { dg-options "-O2 -fdelete-null-pointer-checks -fisolate-erroneous-paths-attribute -fdump-tree-isolate-paths -fdump-tree-ccp3" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */


extern void foo(void *) __attribute__ ((__nonnull__ (1)));

int z;

void
com (int a)
{
    foo (a == 42 ? &z  : (void *) 0);
}

void
bar (void)
{
  foo ((void *)0);
}

/* We testing that the path isolation code can take advantage of the
   returns non-null attribute to isolate a path where NULL flows into
   a return statement.

   We also verify that after isolation phi-cprop simplifies the
   return statement so that it returns &z directly. */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "isolate-paths"} } */
/* { dg-final { scan-tree-dump-times "foo .&z.;" 1 "ccp3"} } */


