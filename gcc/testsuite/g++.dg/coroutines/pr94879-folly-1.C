//  { dg-additional-options  "-fpreprocessed -w" }

namespace std {
template <typename a> a b(a &&);
template <typename c> struct d { c e; };
template <typename f, typename> struct coroutine_traits : f {};
template <typename = void> struct coroutine_handle;
template <> struct coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
template <typename> struct coroutine_handle : coroutine_handle<> {
  static coroutine_handle from_address(void*);
  void* address();
};
struct g {};
} // namespace std

class h {};
class i {
  i(i &&);
};

namespace ac {
template <typename> class ad {
public:
  bool await_ready() noexcept;
  void await_resume() noexcept;
  void await_suspend(std::coroutine_handle<>) noexcept;
  i ae;
};
} // namespace ac

template <typename ab> ac::ad<ab> operator co_await(ab) noexcept;
class j {
  class l {};

public:
  std::g initial_suspend();
  l final_suspend() noexcept;
};
class n;
class m : public j {
public:
  n get_return_object();
  void unhandled_exception();
};
class n {
public:
  using promise_type = m;
};
std::d<h> k;
void a() {
  auto am = k;
  [&]() -> n { co_await std::b(am.e); };
}
