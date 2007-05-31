// { dg-options "--std=c++0x" }
// { dg-do link }

struct S {};
struct T
{
  T(S && s_) : s(s_) {}
  S && get() { return s; }
  operator S&&() { return s; }
  S && s;
};

void named(S const &) {}
void named(S&&);

void unnamed(S const &);
void unnamed(S&&) {}

void f(S && p)
{
  S && s(p);
  T t(s);

  named(s);                          // variable reference
  named(p);                          // parameter reference
  named(t.s);                        // class member access

  unnamed(t.get());                  // function return
  unnamed(t);                        // implicit conversion
  unnamed(static_cast<S&&>(s));      // cast to rvalue
}

int main()
{
}
