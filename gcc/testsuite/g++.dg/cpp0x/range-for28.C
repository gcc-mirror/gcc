// PR c++/59165
// { dg-require-effective-target c++11 }

namespace std {
int* begin(int i) { return (int*)0; }
int* end(int i) { return (int*)0; }
}

int main() {
  for (int a : 10) { }  // { dg-error "was not declared" }
}
