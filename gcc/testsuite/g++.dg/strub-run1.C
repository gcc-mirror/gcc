// { dg-do run }
// { dg-options "-fstrub=internal" }
// { dg-require-effective-target strub }

// Check that we don't get extra copies.

struct T {
  T &self;
  void check () const { if (&self != this) __builtin_abort (); }
  T() : self (*this) { check (); }
  T(const T& ck) : self (*this) { ck.check (); check (); }
  ~T() { check (); }
};

T foo (T q) { q.check (); return T(); }
T bar (T p) { p.check (); return foo (p); }

int main () {
  bar (T()).check ();
}
