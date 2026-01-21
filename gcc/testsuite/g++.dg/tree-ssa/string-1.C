/* { dg-do compile } */
/* { dg-options "-O3 -std=c++20 -fdump-tree-optimized" } */
// { dg-skip-if "required hosted libstdc++ for string" { ! hostedlib } }

#include <string>
std::string
test (std::string &a)
{
	return a;
}
/* { dg-final { scan-tree-dump-not "throw" "optimized" } } */
