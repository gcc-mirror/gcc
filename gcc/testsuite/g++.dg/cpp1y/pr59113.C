// PR c++/59113
// { dg-do compile { target c++1y } }

void foo()
{
  void bar(auto) {} // { dg-error "function-definition|auto|not permitted" }
}

auto i = 0;
