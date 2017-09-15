// { dg-do compile }
// { dg-options "-std=c++17 -fconcepts" }

template<typename T>
  concept bool Class () { return __is_class(T); }

// Allow a requires-expression with no parms.
template<typename T>
  concept bool C = requires { typename T::type; };

void f1(auto a) requires Class<decltype(a)>() { }
void f2(auto a) requires requires (decltype(a) x) { -x; } { }

struct S { } s;

// Allow non-type template parms as constraints.
template<bool B> requires B struct S0; // OK

template<int N> requires N struct S1 { };      // { dg-error "does not have type" }
template<int N> requires N == 0 struct S2 { }; // OK

template<typename T, T X> requires X struct S3 { }; // OK
S3<int, 0> s3a;      // { dg-error "constraint failure|does not have type" }
S3<bool, false> s3b; // { dg-error "constraint failure" }

int main() {
  f1(s);
  f2(0);
}
