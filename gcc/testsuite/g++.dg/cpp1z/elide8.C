// PR c++/110441
// { dg-do compile { target c++11 } }

struct immovable {
  immovable(immovable &&) = delete;
};

struct A {
  static immovable f();
};

immovable f() {
  immovable m = A().f(); // { dg-error "deleted" "" { target c++14_down } }
  return A().f(); // { dg-error "deleted" "" { target c++14_down } }
}

struct B {
  A* operator->();
};

immovable g() {
  B b;
  immovable m = b->f(); // { dg-error "deleted" "" { target c++14_down } }
  return b->f(); // { dg-error "deleted" "" { target c++14_down } }
}
