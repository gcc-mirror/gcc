// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" } */

struct C { int i; }__attribute__((may_alias)) ;

C a, b;

int main()
{
  a = static_cast <C&> (b);
}

// { dg-final { scan-tree-dump "{ref-all}\\\)&b\];" "optimized" } } */
