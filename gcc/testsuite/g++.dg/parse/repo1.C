// { dg-options "-frepo" }

extern "C" inline void f() {}

int main () {
  f();
}

// { dg-final { cleanup-repo-files } }
