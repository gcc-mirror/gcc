// PR c++/94264
// { dg-do compile { target c++17 } }

int main() {
  using T = int[];
  T{1, 2} == nullptr;
}
