// PR c++/68965
// { dg-do compile { target c++14 } }
// { dg-options "-Wall -Wextra" }

auto count = [](auto&&... xs)
{
    return sizeof...(xs);
};

struct count_struct
{
    template<typename... Ts>
    auto operator()(Ts&&... xs)
    {
        return sizeof...(xs);
    }
};

int main()
{
    count(1,2,3);
    count_struct{}(1,2,3);
}
