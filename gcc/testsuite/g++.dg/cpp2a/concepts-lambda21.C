// PR c++/115561
// { dg-do compile { target c++20 } }

template<typename _Tp>
auto declval() noexcept -> _Tp&&;

template<bool, typename _Tp = void>
struct enable_if
{ };

template<typename _Tp>
struct enable_if<true, _Tp>
{ using type = _Tp; };

template<bool _Cond, typename _Tp = void>
using enable_if_t = typename enable_if<_Cond, _Tp>::type;

template<typename _Tp>
struct is_void
{ static constexpr bool value = false;  };

template<typename Fun, typename... Args>
using invoke_result_t =
    decltype(declval<Fun>()(declval<Args>()...));

template<typename R>
using iter_reference_t = decltype(*declval<R &>());

struct iter_move_fn
{
    template<typename I>
    constexpr
    auto operator() (I &&i)  -> void;
} iter_move;

template<typename I>
using iter_rvalue_reference_t = decltype(iter_move(declval<I &>()));

template<class, class>
concept same_as = true;

template<typename I>
concept readable_concept_ =
        same_as<iter_rvalue_reference_t<I const>, iter_rvalue_reference_t<I>>;

template<typename I>
concept indirectly_readable =
    readable_concept_<enable_if_t<true, I>>;

template<typename Fun, typename I>
using indirect_result_t =
    enable_if_t<indirectly_readable<I>,
                        invoke_result_t<Fun, iter_reference_t<I>>>;

template<typename I, typename Fun>
concept transformable =
   (!is_void<indirect_result_t<Fun &, I>>::value);

template<typename I, typename Fun >
    requires transformable<I, Fun>
constexpr void transform(I, Fun)
{
}

void foo()
{
    struct B {};
    (void) transform((B*)nullptr, [](B) {return 0; });
}
