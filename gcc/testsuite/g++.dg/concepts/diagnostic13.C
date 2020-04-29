// PR c++/94830
// { dg-do compile { target concepts } }

template<typename T, typename R>
  concept c = __is_same(T, R); // { dg-message "with T = int; R = char" }

template<typename T, typename R>
  requires c<T,R>
void foo() { }

void bar()
{
  foo<int, char>(); // { dg-error "unsatisfied constraints" }
}
