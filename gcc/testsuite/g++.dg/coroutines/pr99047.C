#include <optional>
#include <coroutine>

template <typename T>
struct [[nodiscard]] task {
    struct promise_type  {
        std::suspend_always initial_suspend() {
            return {};
        }
        auto final_suspend() noexcept {
            struct awaiter {
#if 1
                std::false_type await_ready() noexcept {
                    return {};
                }
#else
                bool await_ready() noexcept {
                    return false;
                }
#endif
                std::coroutine_handle<> await_suspend(std::coroutine_handle<>) noexcept {
                    return next;
                }
                void await_resume() noexcept {
                }
                std::coroutine_handle<> next;
            };
            return awaiter{next};
        }

        void unhandled_exception() noexcept {
            std::terminate();
        }
        auto get_return_object() {
            return task(this);
        }
        auto coro() {
            return std::coroutine_handle<promise_type>::from_promise(*this);
        }
        void return_value(T val) {
            result.emplace(std::move(val));
        }

        std::coroutine_handle<> next;
        std::optional<T> result;
    };

    task(task&& source) : p(std::exchange(source.p, nullptr)) {}
    explicit task(promise_type* p) : p(p) {}
    ~task() {
        if (p)
            p->coro().destroy();
    }

    bool await_ready() noexcept {
        return p->coro().done();
    }
    std::coroutine_handle<> await_suspend(std::coroutine_handle<> next) noexcept {
        p->next = next;
        return p->coro();
    }
    const T& await_resume() const& noexcept {
        return *p->result;
    }

    promise_type* p;
};

task<int> five() {
    co_return 5;
}

task<int> six() {
    co_return co_await five() + 1;
}


int main() {
    auto task = six();
    task.p->next = std::noop_coroutine();
    task.p->coro().resume();
    return *task.p->result;
}
