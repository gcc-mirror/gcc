/* { dg-do compile } */
/* { dg-options "-O3 -std=c++20 -fdump-tree-optimized" } */
#include <string>
std::string
test (std::string &a)
{
	return a;
}
/* { dg-final { scan-tree-dump-not "throw" "optimized" } } */
