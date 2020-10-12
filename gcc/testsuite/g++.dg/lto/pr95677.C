// PR c++/95677

// { dg-do link }
// { dg-require-effective-target lto }
// { dg-options "-flto" }



namespace {
  void foo() {
    extern int xx; // injects a *static*
    xx = 0;
  }
  int xx = 1;
}

int main() {
  xx = 2;
}
