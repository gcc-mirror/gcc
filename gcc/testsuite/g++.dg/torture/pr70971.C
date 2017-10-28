// { dg-additional-options "-std=c++14" }

template<typename Signature>
class function;

template<typename R, typename... Args>
class invoker_base
{
 public:
  virtual ~invoker_base() { }
};

template<typename F, typename R, typename... Args>
class functor_invoker : public invoker_base<R, Args...>
{
 public:
  explicit functor_invoker(const F& f) : f(f) { }
 private:
  F f;
};

template<typename R, typename... Args>
class function<R (Args...)> {
 public:
  template<typename F>
  function(const F& f) : invoker(0) {
    invoker = new functor_invoker<F, R, Args...>(f); 
  }
  ~function() {
    if (invoker)
      delete invoker;
  }
 private:
  invoker_base<R, Args...>* invoker;
};

template<typename>
struct unique_ptr { };

struct A {};
template <class...> struct typelist {};
template <class... Cs> unique_ptr<A> chooseB(typelist<Cs...>);
template <class... Cs, class Idx, class... Rest>
unique_ptr<A> chooseB(typelist<Cs...> choices, Idx, Rest... rest) {
  auto f = [=](auto) { return [=] { return chooseB(choices, rest...); }; };
  function<unique_ptr<A>()> fs[]{f(Cs{})...};
}
main() { chooseB(typelist<double, char>{}, 0, 1, 2); }
