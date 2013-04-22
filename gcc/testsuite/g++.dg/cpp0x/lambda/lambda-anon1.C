// DR 1612
// { dg-require-effective-target c++11 }

int main() {
  static int result;
  struct A { int x; };
  struct B { int y; };
  union {
    A a; B b;
  };
  a.x = 1;
  [=]() mutable {
    a.x = 2;			// { dg-error "anonymous union" }
    result = b.y;		// { dg-error "anonymous union" }
  }();
  if (result == 1) return 0;
  throw 0;
}
