// { dg-additional-options "-fcontracts -fcontract-continuation-mode=on" }
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>
#include <coroutine>

// In order to test the contract violation diagnostic, we have to set
// -fcontract-continuation-mode=on; this means that the code will emit
// the message below - but before that the contract should have been checked.
void process(int from, int to)
{
  if (from > to)
    std::cout << "would have been a disaster!" << std::endl;
}

template <typename T>
struct generator
{
    struct promise_type
    {
        template <typename... Args>
        promise_type(Args&&... args) {
            std::cout << "promise init" << std::endl;
            process(args...);
        }

        std::suspend_always yield_value(T) { return {}; }

        std::suspend_always initial_suspend() const noexcept { return {}; }
        std::suspend_never final_suspend() const noexcept { return {}; }
        void unhandled_exception() noexcept {}

        generator<T> get_return_object() noexcept { return {}; }
    };
};

namespace std {
template <typename T, typename... Args>
struct coroutine_traits<generator<T>, Args...>
{
    using promise_type = typename generator<T>::promise_type;
};

};

generator<int> seq(int from, int to) [[pre: from <= to]]

{
    std::cout << "coro initial" << std::endl;
    for (int i = from; i <= to; ++i) {
        co_yield i;
        std::cout << "coro resumed" << std::endl;
    }
}

int main() { 
    std::cout << "main initial" << std::endl;
    generator<int> s = seq(10, 5);
    (void)s;
    std::cout << "main continues" << std::endl;
}

// { dg-output "contract violation in function seq at .*.C:47: from \<= to.*(\n|\r\n|\r)" }
