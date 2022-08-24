// PR c++/102045
// { dg-do link { target c++17 } }

template<typename T>
struct span
{
  template<unsigned long N>
  constexpr span(T (&a)[N]) : data(a), len(N) { }
  constexpr bool empty() const { return len == 0; }
  T* data;
  unsigned long len;
};

struct byte_writer: span<char> {
  constexpr void do_something() noexcept {
    (void)this->empty();
  }
};

int main() {
  char array[1];
  auto writer = byte_writer{array};
  writer.do_something();
}
