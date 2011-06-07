// { dg-do assemble  }
// { dg-prune-output "note" }

// Adapted from testcase by Corey Kosak <kosak@cs.cmu.edu>

template<class T>
struct moo_t {
  struct cow_t {};
};

template<class T> void foo(typename moo_t<T>::cow_t) {}

template<class T> void foo(moo_t<T>) {
  typename moo_t<T>::cow_t p;
  foo(p); // { dg-bogus "" "" { xfail *-*-* } }  - no matching function for call - 
}

int main() {
  moo_t<int> x;
  foo(x); // { dg-bogus "" "" { xfail *-*-* } }  - required from here -
}
