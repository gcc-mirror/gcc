// PR c++/50437
// { dg-do compile { target c++11 } }

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
