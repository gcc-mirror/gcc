// { dg-do run }
// { dg-options "-O2" }

int i;

struct S {
  S ();
  S (const S&);
  ~S ();
};

S::S () { ++i; }
S::S (const S&) { ++i; }
S::~S () { --i; }

inline void f (S) {
}

int main () {
  {
    S s;
    f (s);
  }

  return i;
}

