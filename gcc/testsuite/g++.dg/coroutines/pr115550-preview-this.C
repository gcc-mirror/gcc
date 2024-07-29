// PR c++/115550 - wrong deduction when passing *this to promise ctor template

#include <coroutine>

template <typename T> struct remove_reference { using type = T; };
template <typename T> struct remove_reference<T&> { using type = T; };
template <typename T> struct remove_reference<T&&> { using type = T; };
template <typename T> using remove_reference_t = remove_reference<T>::type;

template <typename, typename>
struct is_same { static inline constexpr bool value = false; };
template <typename T>
struct is_same<T, T> { static inline constexpr bool value = true; };

template <typename T, typename U>
concept same_as = is_same<T, U>::value;

struct coroutine
{
    struct promise_type
    {
        template <typename Arg>
        explicit promise_type(Arg&&)
        {
            static_assert(same_as<
                remove_reference_t<remove_reference_t<Arg>>,
                remove_reference_t<Arg>
            >);
        }

        coroutine get_return_object() { return {}; }

        std::suspend_never initial_suspend() noexcept { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }

        void return_void() {}
        void unhandled_exception() {throw;}
    };
};

struct x
{
    coroutine f()
    {
        co_return;
    }
};
