// { dg-options "-frepo" }
// { dg-require-host-local "" }
// { dg-warning "is deprecated and will be removed in a future release" "" { target *-*-* } 0 }

extern "C" inline void f() {}

int main () {
  f();
}

// { dg-final { cleanup-repo-files } }
