// PR c++/50437
// { dg-options -std=c++0x }

template <typename T>
void f()
{
    auto g = [](T t){ return t == 0; };
    g(T());
}

int main()
{
    f<int>();
}
