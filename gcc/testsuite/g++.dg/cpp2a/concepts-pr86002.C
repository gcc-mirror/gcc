// PR c++/86002
// { dg-do compile { target c++20 } }

struct X {};
struct Y { int i; };

template <typename T>
int f(T t)
{
    if constexpr (requires { t.i; })
        return t.i;
    else
        return {};
}

int main()
{
    return f(X{}) + f(Y{});
}
