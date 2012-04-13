// PR c++/52915

struct S {
  int val;
  S(int v) : val(v) {}
};

void f() {
  union { S a; };		// { dg-error "constructor|no match" }
}
