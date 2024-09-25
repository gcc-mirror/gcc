// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
// A basic implementation of TR1's function using variadic teplates
// Contributed by Douglas Gregor <doug.gregor@gmail.com>
#include <cassert>

template<typename Signature>
class function;

template<typename R, typename... Args>
class invoker_base
{
 public:
  virtual ~invoker_base() { }
  virtual R invoke(Args...) = 0;
  virtual invoker_base* clone() = 0;
};

template<typename F, typename R, typename... Args>
class functor_invoker : public invoker_base<R, Args...>
{
 public:
  explicit functor_invoker(const F& f) : f(f) { }
  R invoke(Args... args) { return f(args...); }
  functor_invoker* clone() { return new functor_invoker(f); }

 private:
  F f;
};

template<typename R, typename... Args>
class function<R (Args...)> {
 public:
  typedef R result_type;

  function() : invoker (0) { }

  function(const function& other) : invoker(0) {
    if (other.invoker) 
      invoker = other.invoker->clone();
  }

  template<typename F>
  function(const F& f) : invoker(0) {
    invoker = new functor_invoker<F, R, Args...>(f); 
  }

  ~function() {
    if (invoker)
      delete invoker;
  }

  function& operator=(const function& other) {
    function(other).swap(*this);
    return *this;
  }

  template<typename F>
  function& operator=(const F& f) {
    function(f).swap(*this);
    return *this;
  }

  void swap(function& other) {
    invoker_base<R, Args...>* tmp = invoker;
    invoker = other.invoker;
    other.invoker = tmp;
  }

  result_type operator()(Args... args) const {
    assert(invoker);
    return invoker->invoke(args...);
  }

 private:
  invoker_base<R, Args...>* invoker;
};

struct plus {
  template<typename T> T operator()(T x, T y) { return x + y; }
};

struct multiplies {
  template<typename T> T operator()(T x, T y) { return x * y; }
};

int main()
{
  function<int(int, int)> f1 = plus();
  assert(f1(3, 5) == 8);

  f1 = multiplies();
  assert(f1(3, 5) == 15);

  return 0;
}
