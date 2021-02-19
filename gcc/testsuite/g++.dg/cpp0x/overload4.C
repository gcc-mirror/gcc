// PR c++/96926
// { dg-do compile { target c++11 } }

namespace std
{
  template<typename _Tp, _Tp __v>
    struct integral_constant
    {
      static constexpr _Tp value = __v;
      typedef integral_constant<_Tp, __v> type;
    };

  template<typename _Tp, _Tp __v>
    constexpr _Tp integral_constant<_Tp, __v>::value;

  typedef integral_constant<bool, true> true_type;
  typedef integral_constant<bool, false> false_type;

  template<bool __v>
    using bool_constant = integral_constant<bool, __v>;

  template<bool, typename, typename>
    struct conditional;

  template<typename...>
    struct __and_;

  template<>
    struct __and_<>
    : public true_type
    { };

  template<typename _B1>
    struct __and_<_B1>
    : public _B1
    { };

  template<typename _B1, typename _B2>
    struct __and_<_B1, _B2>
    : public conditional<_B1::value, _B2, _B1>::type
    { };

  template<typename _B1, typename _B2, typename _B3, typename... _Bn>
    struct __and_<_B1, _B2, _B3, _Bn...>
    : public conditional<_B1::value, __and_<_B2, _B3, _Bn...>, _B1>::type
    { };

  template<typename _Tp, typename... _Args>
    struct is_constructible
    : public bool_constant<__is_constructible(_Tp, _Args...)>
    {
    };

  template<bool, typename _Tp = void>
    struct enable_if
    { };

  template<typename _Tp>
    struct enable_if<true, _Tp>
    { typedef _Tp type; };

  template<bool _Cond, typename _Tp = void>
    using __enable_if_t = typename enable_if<_Cond, _Tp>::type;

  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct conditional
    { typedef _Iftrue type; };


  template<typename _Iftrue, typename _Iffalse>
    struct conditional<false, _Iftrue, _Iffalse>
    { typedef _Iffalse type; };


  template<bool, typename... _Types>
    struct _TupleConstraints
    {
      template<typename... _UTypes>
        static constexpr bool __is_implicitly_constructible()
        {
          // is_constructible is incomplete here, but only when
          // it is also instantiated in __is_explicitly_constructible 
          return __and_<is_constructible<_Types, _UTypes>...,
                 true_type
                   >::value;
        }

      template<typename... _UTypes>
        static constexpr bool __is_explicitly_constructible()
        {
#if FIX
          return false;
#else
          return __and_<is_constructible<_Types, _UTypes>...,
                 false_type
                   >::value;
#endif
        }
    };

  template<typename... _Elements>
    class tuple
    {
      template<bool _Cond>
        using _TCC = _TupleConstraints<_Cond, _Elements...>;

      template<bool _Cond, typename... _Args>
        using _ImplicitCtor = __enable_if_t<
        _TCC<_Cond>::template __is_implicitly_constructible<_Args...>(),
        bool>;

      template<bool _Cond, typename... _Args>
        using _ExplicitCtor = __enable_if_t<
        _TCC<_Cond>::template __is_explicitly_constructible<_Args...>(),
        bool>;

    public:

      template<bool _NotEmpty = true,
        _ImplicitCtor<_NotEmpty, const _Elements&...> = true>
          constexpr
          tuple(const _Elements&... __elements)
          { }

      template<bool _NotEmpty = true,
        _ExplicitCtor<_NotEmpty, const _Elements&...> = false>
          explicit constexpr
          tuple(const _Elements&... __elements)
          { }
    };
}

// first example

template <typename SessionT>
struct SomeQuery {
    SessionT& session_;
    SomeQuery(SessionT& session) : session_(session) {}
};

template <typename SessionT>
struct Handler {
    std::tuple<SomeQuery<SessionT>> queries_;
    Handler(SessionT& session) : queries_(session) {}
};

struct Session {
    Handler<Session> handler_;
    Session() : handler_{*this} {}
};

int main() {
    Session session;
}
static_assert(std::is_constructible<SomeQuery<Session>, const SomeQuery<Session>&>::value, "");

// second example

template <typename T>
class DependsOnT
{
public:
    DependsOnT(T&) {}
};

class Test
{
public:
    Test() : test_{*this} {}

private:
    std::tuple<DependsOnT<Test>> test_;
};
static_assert(std::is_constructible<DependsOnT<Test>, const DependsOnT<Test>&>::value, "");
