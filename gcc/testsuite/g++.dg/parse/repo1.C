// { dg-options "-frepo" }
// { dg-require-host-local "" }

extern "C" inline void f() {}

int main () {
  f();
}

// { dg-final { cleanup-repo-files } }
