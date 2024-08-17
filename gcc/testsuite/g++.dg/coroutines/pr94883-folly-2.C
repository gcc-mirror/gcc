
namespace std {
template <typename a, typename...> struct coroutine_traits : a {};
template <typename = void> struct coroutine_handle;
template <> struct coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
template <typename> struct coroutine_handle : coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
struct b {
  bool await_ready();
  void await_suspend(coroutine_handle<>);
  void await_resume();
};
} // namespace std
class h;
template <typename, typename = int> class k {
public:
  using promise_type = h;
  using i = std::coroutine_handle<>;
  class l {
  public:
    ~l();
    operator bool();
  };
  class m {
  public:
    bool await_ready();
    i await_suspend(std::coroutine_handle<>);
    l await_resume();
  };
  class n {
  public:
    m e(int);
  };
  n ah();
};

template <typename ai, typename aj, typename ak>
k<aj> 
my_coro (k<aj, ak> am, ai) {
  if (auto an = co_await am.ah())
    ;
}

template <typename d> auto ab(int ac, d ad) -> decltype(ad.e(ac));
int f;
class h {
  class j {
  public:
    bool await_ready() noexcept;
    void await_suspend(std::coroutine_handle<>) noexcept;
    void await_resume() noexcept;
  };

public:
  k<int> get_return_object();
  std::b initial_suspend();
  j final_suspend() noexcept;
  void unhandled_exception();
  template <typename g> 
    auto await_transform (g c) { return ab(f, c); }
};

void foo () {
  k<int> a;
  my_coro (a, [] {});
}
