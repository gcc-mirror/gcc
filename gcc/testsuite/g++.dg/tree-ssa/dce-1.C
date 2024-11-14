/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
#include <new>
void test ()
{
	int *a = new (std::nothrow) int;
	delete a;
}
/* { dg-final { scan-tree-dump-not "operator new" "optimized" } } */
