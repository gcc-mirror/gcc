// PR c++/43559

template<typename T, typename U> void f(U&) { }
template<typename T, typename U> void f(T const&) { }

int main()
{
        int a;
        f<int, int const>(a);
}
