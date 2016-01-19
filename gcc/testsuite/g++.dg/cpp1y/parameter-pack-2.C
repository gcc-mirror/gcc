// PR c++/68965
// { dg-do compile { target c++14 } }
// { dg-options "-Wall -Wextra" }

auto count = [](auto&&... xs) // { dg-warning "unused parameter" }
{
};

struct count_struct
{
    template<typename... Ts>
    auto operator()(Ts&&... xs) // { dg-warning "unused parameter" }
    {
    }
};

int main()
{
    count(1,2,3);
    count_struct{}(1,2,3);
}
