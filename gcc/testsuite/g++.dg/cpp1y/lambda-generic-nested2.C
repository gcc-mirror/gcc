// PR c++/82514
// { dg-do compile { target c++14 } }

void g();
template <typename h> void i(h) { g(); }
template <int> void n() {
  [](auto) {
    struct p { };
    i(p{});
  } ('\n');
}

auto f = n<1>;
