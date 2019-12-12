// PR c++/63508
// { dg-do compile { target c++11 } }

template <typename T> class c {
  void mf() { }
  using m = void (c::*) ();
public:
  c() {
    auto x = m{&c::mf};
  }
};

int main() { c<int> o{}; }
