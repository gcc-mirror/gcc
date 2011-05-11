// PR c++/48859
// { dg-do compile }

struct HasConstructor {
  HasConstructor() {}
};

class ConstMember {
  const HasConstructor empty_;
};

void foo() {
  new ConstMember;
}
