// PR c++/53202
// { dg-do run { target c++11 } }

#include <tuple>

template<typename Callable>
  struct Bind_simple
  {
    explicit
    Bind_simple(const Callable& callable)
    : _M_bound(callable)
    { }

    Bind_simple(const Bind_simple&) = default;
    Bind_simple(Bind_simple&&) = default;

    auto operator()() -> decltype((*(Callable*)0)())
    {
      return std::get<0>(_M_bound)();
    }

  private:

    std::tuple<Callable> _M_bound;
  };

template<typename Callable>
  Bind_simple<Callable>
  bind_simple(Callable& callable)
  {
    return Bind_simple<Callable>(callable);
  }

struct thread
{
  struct ImplBase { };

  template<typename T>
    struct Impl : ImplBase {
      T t;
      Impl(T&& t) : t(std::move(t)) { }
    };

  template<typename T>
    thread(T& t)
    {
      auto p = make_routine(bind_simple(t));

      p->t();

      delete p;
    }

  template<typename Callable>
    Impl<Callable>*
    make_routine(Callable&& f)
    {
      return new Impl<Callable>(std::forward<Callable>(f));
    }
};


int c;
class background_hello
{
public:
    background_hello()
    {
      __builtin_printf("default ctor called, this=%p\n", this);
      ++c;
    }

    background_hello(const background_hello &)
    {
      __builtin_printf("copy ctor called\n");
      ++c;
    }

    background_hello(background_hello &&)
    {
      __builtin_printf("move ctor called\n");
      ++c;
    }

    void operator ()() const
    {
      __builtin_printf("void background_hello::operator()() called, this=%p\n", this);
    }

    ~background_hello()
    {
      __builtin_printf("destructor called, this=%p\n", this);
      --c;
    }

};

int main()
{
  {
    background_hello bh;
    thread t(bh);
  }
  if (c != 0)
    __builtin_abort ();
}
