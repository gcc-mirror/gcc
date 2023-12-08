// PR c++/112658
// PR c++/94264
// { dg-do compile { target c++11 } }

void f(int*);

int main() {
  using array = int[];
  f(array{42});
  f((int*)array{42});
}
