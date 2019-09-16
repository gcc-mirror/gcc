// PR c++/82165
// { dg-do compile { target c++11 } }

struct flags {
  enum field { f0, f1, no_field };
  field b0 : 4;
  field b1 : 4;
  field a0, a1;
};

constexpr bool operator!(flags::field f) {
  return f == flags::no_field;
}

#define SA(X) static_assert ((X), #X)

int main() {
  constexpr flags f { flags::f0, flags::f1, flags::f0, flags::f1 };

  SA( flags::f0 == 0 ); // 0
  SA( flags::f1 == 1 ); // 1
  SA( flags::no_field == 2 ); // 2
  SA( !flags::f0 == 0 ); // (!) 0
  SA( !flags::f1 == 0 ); // (!) 0
  SA( !flags::no_field == 1 ); // (!) 1

  SA( f.a0 == 0 ); // 0
  SA( f.a1 == 1 ); // 1
  SA( !f.a0 == 0 ); // (!) 0
  SA( !f.a1 == 0 ); // (!) 0

  SA( f.b0 == 0 ); // 0
  SA( f.b1 == 1 ); // 1
  SA( !f.b0 == 0 ); // expected "(!) 0", but got "1"
  SA( !f.b1 == 0 ); // expected "(!) 0", but got "0"
}
