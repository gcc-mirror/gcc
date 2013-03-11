// PR c++/56567
// { dg-require-effective-target c++11 }

#include <initializer_list>

int main()
{
  []{ return { 1, 2 }; }();	// { dg-error "initializer.list" }
}

// { dg-prune-output "return-statement with a value" }
