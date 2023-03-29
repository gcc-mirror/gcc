#if !__has_include(<coroutine>) \
  && __has_include(<experimental/coroutine>) // for __clang__
#include <experimental/coroutine>
namespace std {
  using namespace std::experimental;
}
#else
#include <coroutine>
#endif
#include <optional>

struct future
{
    using value_type = int;
    struct promise_type;
    using handle_type = std::coroutine_handle<promise_type>;

    handle_type _coroutine;

    future(handle_type h) : _coroutine{h} {}

    ~future() noexcept{
        if (_coroutine) {
            _coroutine.destroy();
        }
    }

    value_type get() {
        auto ptr = _coroutine.promise()._value;
        return *ptr;
    }

    struct promise_type {
        std::optional<value_type> _value = std::nullopt;

        future get_return_object() {
            return future{handle_type::from_promise(*this)};
        }
        void return_value(value_type val) {
            _value = static_cast<value_type &&>(val);
        }
        auto initial_suspend() noexcept {
            class awaiter {
                std::optional<value_type> & value;
            public:
                explicit awaiter(std::optional<value_type> & val) noexcept : value{val} {}
                bool await_ready() noexcept { return value.has_value(); }
                void await_suspend(handle_type) noexcept { }
                value_type & await_resume() noexcept { return *value; }
            };

            return awaiter{_value};
        }
        std::suspend_always final_suspend() noexcept {
            return {};
        }
        //void return_void() {}
        void unhandled_exception() {}
    };
};

future create_future()
{ co_return 2021; }

int main()
{ auto f = create_future(); }
