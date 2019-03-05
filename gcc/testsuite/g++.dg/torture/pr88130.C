/* { dg-do compile } */
/* { dg-options "-flto" } */
/* { dg-require-effective-target lto } */
class a {
public:
  static const long b = 1;
};
struct c {
  enum d { e };
};
class C;
class f {
public:
  f(c::d);
  template <typename g> C operator<=(g);
};
class C {
public:
  template <typename h> void operator!=(h &);
};
void i() {
  f j(c::e);
  try {
    j <= 0 != a::b;
  } catch (...) {
  }
}
