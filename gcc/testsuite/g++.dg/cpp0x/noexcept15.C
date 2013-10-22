// PR c++/50391
// { dg-options -std=c++11 }

#include <type_traits>

template<class Tp>
  struct single
  {
    Tp elem;  // { dg-error "incomplete type" }

    constexpr single(const Tp& e)
    : elem(e) { }

    single(single&& s)
    noexcept(std::is_nothrow_move_constructible<Tp>::value) 
    : elem(s.elem) { }
  };

template<class Tp>
  constexpr single<typename std::decay<Tp>::type>
  make_single(Tp&& x)
  {
    return single<typename std::decay<Tp>::type>(x);
  }

class Blob;  // { dg-error "forward declaration" }

void
foo(Blob *b)
{
  make_single(*b);
}

// { dg-prune-output "include" }
