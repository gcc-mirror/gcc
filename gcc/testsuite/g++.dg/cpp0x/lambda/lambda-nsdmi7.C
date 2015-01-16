// PR c++/64496
// { dg-do compile { target c++11 } }

template <typename> class B;
template <typename W, typename... X>
struct B<W(X...)> { template <typename F> B(F); };
template <typename W, typename... X>
template <typename F>
B<W(X...)>::B(F) {}

int
main()
{
  int a;
  struct A			// { dg-message "lambda in local class" }
  {
    B<void()> l = [=] {
      a;			// { dg-error "not captured" }
    };
  };
  [] {				// { dg-message "capture-default" }
    a;				// { dg-error "not captured" }
  };
  A t;
}
