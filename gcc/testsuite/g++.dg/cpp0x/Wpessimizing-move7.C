// PR c++/106276
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move" }

// Define std::move.
namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

struct A { A(); A(const A&) = delete; A(A&&); };
struct B { B(A); };

static A foo ();

A
fn1 ()
{
  return std::move (A{}); // { dg-warning "moving a temporary object prevents copy elision" }
  return std::move (A()); // { dg-warning "moving a temporary object prevents copy elision" }
  return std::move (foo ()); // { dg-warning "moving a temporary object prevents copy elision" }
}

B fn2 ()
{
  return std::move (A()); // { dg-warning "moving a temporary object prevents copy elision" }
  return std::move (A{}); // { dg-warning "moving a temporary object prevents copy elision" }
  return std::move (foo ()); // { dg-warning "moving a temporary object prevents copy elision" }
}

template <typename T1, typename T2>
T1
fn3 ()
{
  return std::move (T2{}); // { dg-warning "moving a temporary object prevents copy elision" }
}

void
do_fn3 ()
{
  fn3<A, A>();
  fn3<B, A>();
}

char take_buffer;
struct label_text {
  label_text take() { return std::move(label_text(&take_buffer)); } // { dg-warning "moving a temporary object prevents copy elision" }
  label_text(char *);
};
