// PR c++/98231
// { dg-do compile }

template <typename, typename = int> struct basic_ostream {};
namespace N {
  template <typename Char, typename CharTraits, typename T>
  void operator<<(basic_ostream<Char, CharTraits>, T);
}
basic_ostream<char> os;

template<typename T> void
foo (T value)
{
  using N::operator<<;
  os << value;
}
void bar() { foo (1); }
