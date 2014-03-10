// { dg-do compile { target c++11 } }

struct booleable {
  bool data;
  constexpr explicit operator bool() { return data; }
};

constexpr booleable truthy_func() { return {true}; }

void funky() noexcept(truthy_func()) {}

int main() {
  funky();
  return 0;
}
