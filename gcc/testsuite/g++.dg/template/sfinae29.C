// PR c++/90880
// { dg-do compile { target c++11 } }

template <typename T, typename = void>
struct status
{ static const bool value = false; };

template <typename T>
struct status<T, decltype((void)T::member)>
{ static const bool value = true; };

struct s1{int member;};
struct s2{int _member;};

class c1{int member;};
class c2{int _member;};

void
foo()
{
  static_assert(status<s1>::value, "has member");
  static_assert(!status<s2>::value, "has no member");
  static_assert(!status<c1>::value, "has inaccessible member");
  static_assert(!status<c2>::value, "has no member");
}
