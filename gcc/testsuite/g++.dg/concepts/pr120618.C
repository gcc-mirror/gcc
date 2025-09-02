// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

class B{};

template <typename T>
requires (!requires(T t) { { t } -> bool; }) // { dg-error "return-type-requirement is not a type-constraint" }
void foo(T t) {}

int main() {
  B b;
  foo(b); // { dg-prune-output "no matching function" }
}
