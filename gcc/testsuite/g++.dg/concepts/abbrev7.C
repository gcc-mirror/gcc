// PR c++/96106
// { dg-do compile { target concepts } }

template<typename>
struct number {
  friend void add(auto);
};

void add(auto) { }

void foo() {
  number<int> n;
  add(n); // { dg-bogus "ambiguous" }
}
