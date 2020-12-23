template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
  typedef _Tp value_type;
  typedef integral_constant<_Tp, __v> type;
};

template<typename _Tp, _Tp __v>
constexpr _Tp integral_constant<_Tp, __v>::value;

typedef integral_constant<bool, false> false_type;


template<typename _T1>
struct Alien
{
};

template<typename _T1>
bool operator==(const Alien<_T1>& __x, const Alien<_T1>& __y);

template<typename _Rep>
struct duration;

template<bool _DenIsOne>
struct __duration_cast_impl
{
};

template<typename _ToDur, typename _Rep>
long Frob (const duration<_Rep>& __d)
{
  typedef __duration_cast_impl<// finds ::operator==
			       _ToDur::num == 1> __dc;

  return 0;
}

template<typename _Rep>
struct duration
{
public:
  constexpr duration() = default;

  duration (const duration& __d)
  {
    Frob <duration> (__d);
  }
};

template<typename _Tp>
struct __atomic_semaphore
{
  template<typename _Rep>
  bool _M_try_acquire_for (const duration<_Rep>& __rtime) noexcept;

  void _M_release() noexcept;
};

template<long __least_max_value>
class counting_semaphore
{
  __atomic_semaphore<unsigned> _M_sem;

public:
  explicit counting_semaphore() noexcept;

  void release() noexcept (noexcept (_M_sem._M_release ()));
};

class stop_token
{
public:
  counting_semaphore<1> _M_done;
};

bool operator==(const stop_token& __a, const stop_token& __b);
