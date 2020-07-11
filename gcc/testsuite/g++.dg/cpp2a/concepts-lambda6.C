// PR c++/94645
// { dg-do compile { target concepts } }

struct unordered_map {
  int cend() const noexcept;
};

template <typename a> concept HasMapInterface = requires(a t) { t.cend(); };

template <typename Mapper> requires HasMapInterface<decltype(Mapper::map())>
struct l {
  friend void foo(l opt) { ([]() {})(); }
};

struct p {
  static unordered_map map();
};

void g(l<p> *y) { foo(*y); }
