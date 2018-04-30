// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Class () { return __is_class(T); }

void f1(auto a) requires Class<decltype(a)>() { }

// FIXME: This is generating excess errors related to pretty
// printing the trailing requires expression.
void f2(auto a)
  requires requires (decltype(a) x) { -x; }
{ }

struct S { } s;

int main() {
  f1(0); // { dg-error "cannot call" }
  f2((void*)0); // { dg-error "cannot call" }
}
