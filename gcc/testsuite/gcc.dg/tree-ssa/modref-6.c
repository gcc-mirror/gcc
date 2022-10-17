/* { dg-options "-O2 -fdump-tree-modref1 -fdump-tree-optimized"  } */
/* { dg-do compile } */
int c;
__attribute__ ((noinline))
int *test (int *b)
{
  c++;
  return *b ? &c : 0;
}
__attribute__ ((noinline, pure))
int *pure_test (int *b)
{
  return *b && c ? &c : 0;
}
__attribute__ ((noinline, const))
int *const_test (int *b)
{
  return b ? &c : 0;
}
void escape (int *);

int test2()
{
   int a = 42;
   escape (test (&a));
   escape (pure_test (&a));
   escape (const_test (&a));
   return a;
}
/* Flags for normal call.  */
/* { dg-final { scan-tree-dump "parm 0 flags: no_direct_clobber no_indirect_clobber no_direct_escape no_indirect_escape not_returned_directly not_returned_indirectly no_indirect_read" "modref1"  } } */
/* Flags for pure call.  */
/* { dg-final { scan-tree-dump "parm 0 flags: not_returned_directly not_returned_indirectly no_indirect_read" "modref1"  } } */
/* Flags for const call.  */
/* { dg-final { scan-tree-dump "parm 0 flags: not_returned_directly" "modref1"  } } */
/* Overall we want to make "int a" non escaping.  */
/* { dg-final { scan-tree-dump "return 42" "optimized"  } } */
