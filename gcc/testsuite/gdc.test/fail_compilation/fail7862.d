/*
TEST_OUTPUT:
---
A: false
A: false
fail_compilation/fail7862.d(26): Error: template instance nonExistent!() template 'nonExistent' is not defined
fail_compilation/fail7862.d(25): Error: template instance fail7862.B!(A) error instantiating
---
*/

// 7862

template B(T) {
  mixin(
    {
      foreach (name; __traits(derivedMembers, T)) {}
      return "struct B {}";
    }()
  );
}

struct A {
  pragma(msg, "A: " ~ (__traits(compiles, B!A) ? "true" : "false"));
  pragma(msg, "A: " ~ (__traits(compiles, B!A) ? "true" : "false"));
  B!A c;
  static if (nonExistent!()) {}
}

auto d = A.init.c;

