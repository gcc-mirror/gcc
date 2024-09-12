// { dg-additional-options "-fcontracts -fcontract-continuation-mode=on" }
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>
#include <coroutine>


template <typename T>
struct generator
{
    struct promise_type
    {
        std::suspend_always yield_value(T) { return {}; }

        std::suspend_always initial_suspend() const noexcept { return {}; }
        std::suspend_never final_suspend() const noexcept { return {}; }
        void unhandled_exception() noexcept {}

        generator<T> get_return_object() noexcept { return {}; }
    };

    bool is_valid() { return false; }
};

namespace std {
template <typename T, typename... Args>
struct coroutine_traits<generator<T>, Args...>
{
    using promise_type = typename generator<T>::promise_type;
};

};

generator<int> val(int v) 
[[post g: g.is_valid()]]
{
    std::cout << "coro initial" << std::endl;
    co_yield v;
    std::cout << "coro resumed" << std::endl;
}

int main() { 
    std::cout << "main initial" << std::endl;
    generator<int> s = val(1);
    (void)s;
    std::cout << "main continues" << std::endl;
}

// { dg-output "contract violation in function val at .*.C:36: g.is_valid().*(\n|\r\n|\r)" }
