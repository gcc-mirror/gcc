// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-skip-if "dkms are not final links" { vxworks_kernel } }

struct A {
  A();
};

A::A() {}

template <typename T>
struct B : public A {
  B() {} // { dg-bogus "" }
};

B<int> b;

int main () {}

// { dg-final { cleanup-repo-files } }
