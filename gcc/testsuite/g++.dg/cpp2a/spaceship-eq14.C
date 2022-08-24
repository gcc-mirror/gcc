// PR c++/106361
// { dg-do compile { target c++20 } }

struct foo {
  int x;
};

struct bar {
  foo f;			// { dg-error "operator==" }
  friend bool operator==(const bar& a, const bar& b);
};

bool operator==(const bar& a, const bar& b) = default;

int main() {
  return bar{} == bar{};	// { dg-error "deleted" }
}
