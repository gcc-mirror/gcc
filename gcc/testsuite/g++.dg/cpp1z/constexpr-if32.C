// PR c++/81676 - bogus -Wunused warnings in constexpr if.
// { dg-do compile { target c++17 } }
// { dg-options "-Wall -Wextra" }

int main()
{
    auto f = [](auto a, auto b) {
        if constexpr (sizeof(b) == 1) {
            return a;
        } else {
            return b;
        }
    };

    return f(1, 1) + f(1, 'a');
}
