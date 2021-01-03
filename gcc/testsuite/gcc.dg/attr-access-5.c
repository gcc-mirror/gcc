/* { dg-do compile }
   { dg-options "-fdump-tree-gimple" } */

__attribute__ ((aligned (32)))
__attribute__ ((access (write_only, 2, 1)))
void f (int n, void *p)
{
  __builtin_memset (p, 0, n);
}

/* Verify the DECL_ATTRIBUTE "aligned" is mentioned:
   { dg-final { scan-tree-dump "__attribute__\\(\\(aligned" "gimple" } }
   and the TYPE_ATTRIBUTE "access" is also mentioned:
   { dg-final { scan-tree-dump "__attribute__\\(\\(access" "gimple" } }
   and the function signature including its return type is mentioned:
   { dg-final { scan-tree-dump "void f *\\(int n, void *\\* *p\\)" "gimple" } } */
