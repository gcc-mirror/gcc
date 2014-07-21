// PR c++/60848
// { dg-do compile { target c++11 } }

namespace std
{
  struct initializer_list {};	// { dg-message "initializer_list" }
}

void foo(std::initializer_list &);

void f()
{
  foo({1, 2});
}

// { dg-prune-output "compilation terminated" }
