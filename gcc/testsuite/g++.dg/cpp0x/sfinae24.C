// PR c++/49058
// This error is not subject to SFINAE because it doesn't happen in the
// deduction context.
// { dg-options -std=c++11 }
// { dg-prune-output "note" }

template<typename T> T val();

struct F1
{
    void operator()();
};

template<typename F>
struct Bind
{
    template<typename R
      = decltype( val<F>()( ) )>
    R f();

    template<typename R
      = decltype( val<const F>()( ) )> // { dg-error "no match" }
    R f() const;
};

int main()
{
  Bind<F1> b;
}
