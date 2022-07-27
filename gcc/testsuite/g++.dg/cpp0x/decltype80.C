// PR c++/85493
// { dg-do compile { target c++11 } }

struct no_def {
  no_def() = delete;
};

template<class...>
int foo() = delete;

template<class>
void test() {
  decltype(no_def()) a1; // { dg-error "deleted" }
  decltype(no_def(1,2,3)) a2; // { dg-error "no match" }
  decltype(foo<>()) a3; // { dg-error "deleted" }
}
