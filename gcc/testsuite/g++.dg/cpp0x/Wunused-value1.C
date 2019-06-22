// PR c++/90881
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

namespace std {
  struct true_type { static const bool value = true; };
  struct false_type { static const bool value = false; };
}

template <typename T, typename = void> struct status : std::false_type{};

template <typename T> struct status<T, decltype(T::member, void())> : std::true_type {}; // { dg-bogus "left operand of comma operator has no effect" }

struct s1{int member;};
struct s2{int _member;};

int main(){
	static_assert(status<s1>::value, "has member");
	static_assert(!status<s2>::value, "has no member");
}
