// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe" }
// NOTE this should switch to run when the changes are resolved.
// { dg-do compile { target c++26 } }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// { dg-prune-output "during RTL pass: expand" }
// { dg-ice "expand_expr_addr_expr_1" }

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

    bool is_valid() const { return false; }
};

namespace std {
template <typename T, typename... Args>
struct coroutine_traits<generator<T>, Args...>
{
    using promise_type = typename generator<T>::promise_type;
};

};

generator<int> val(int v) 
  post (g: g.is_valid())
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

// { dg-output "contract violation in function generator<int> val.int. at .*.C:36: g.is_valid().*(\n|\r\n|\r)" }
