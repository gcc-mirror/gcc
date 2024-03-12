// PR c++/110463
// { dg-do compile { target c++11 } }

struct U {
  mutable int x = 1;
};

struct V {
  mutable int y = 1+1;
};

int main() {
  constexpr U u = {};
  constexpr int x = u.x; // { dg-error "mutable" }

  constexpr V v = {};
  constexpr int y = v.y; // { dg-error "mutable" }
}
