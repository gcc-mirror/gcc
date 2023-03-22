// PR c++/109241
// { dg-do compile { target c++14 } }
// { dg-options "" } no pedantic

void g() {
  [](auto) {
    [](auto) {
      ({
        struct A {};
      });
    };
  }(1);
}
