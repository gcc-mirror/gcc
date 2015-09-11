// PR c++/52659
// { dg-do compile { target c++11 } }

struct sometype {
  sometype();
};

sometype::sometype() = delete;  // { dg-error "deleted" }
