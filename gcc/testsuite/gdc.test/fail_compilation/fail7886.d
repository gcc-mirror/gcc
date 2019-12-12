// 7886

struct A {
  static if (__traits(derivedMembers, A).length) {}
}
