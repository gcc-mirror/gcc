// PR c++/57626
// { dg-do compile { target c++11 } }

struct symbol_set{};

template <typename T, template <typename ...> class TT, typename ... Args>
using bar = void(T::*)(TT<Args...> &, const symbol_set &);

struct converter
{
  template <typename Term, typename ... Args,
	    typename = decltype(bar<Term,Args...>(&Term::multiply))>  // { dg-error "pack expansion" }
  converter(const Term &);
};
