// { dg-do compile { target c++11 } }

#include <initializer_list>

extern "C" int printf (const char *, ...);

using size_t = decltype(sizeof(0));

template <typename T> class ArrayRef {
public:
  using size_type = size_t;

private:
  /// The start of the array, in an external buffer.
  const T *Data = nullptr;

  /// The number of elements.
  size_type Length = 0;

public:
  /// Construct an ArrayRef from a std::initializer_list.
  /*implicit*/ ArrayRef(const std::initializer_list<T> &Vec)
      : Data(Vec.begin() == Vec.end() ? (T *)nullptr : Vec.begin()), // { dg-warning initializer_list }
        Length(Vec.size()) {}

  const T &operator[](size_t Index) const { return Data[Index]; }
};

int main() {
  const ArrayRef<int> Foo = {42};
  printf ("Foo %d\n", Foo[0]);
}
