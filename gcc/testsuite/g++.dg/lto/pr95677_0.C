// PR c++/95677

// { dg-lto-do link }
// { dg-lto-options { -flto } }



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
