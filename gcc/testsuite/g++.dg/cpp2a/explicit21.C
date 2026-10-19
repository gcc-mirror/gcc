// PR c++/127627
// { dg-do compile { target c++20 } }

template <class T>
struct X
{
  X() = default;
  X(const X&) { throw; }
private:
  explicit X(const X&) requires (T::value) = default;
};

struct S { static constexpr bool value = true; };

int main()
{
    const X<S> x;
    X<S> x2 = x;
}
