// Test PR 15736 fix
// Contributed by Devang Patel <dpatel@apple.com>
// { dg-do compile }


struct B {
  int n;
};

struct A : B {
  using B::n;
};
