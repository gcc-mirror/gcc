// PR c++/81486
// { dg-do compile { target c++17 } }

template <class T>
struct C {
  C(T);
};

template <>
struct C<void> { };

C() -> C<void>;

int
main()
{
  auto a = C{};
  auto b = C();
}
