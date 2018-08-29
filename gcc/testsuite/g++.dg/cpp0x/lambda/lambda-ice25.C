// PR c++/83204
// { dg-do compile { target c++11 } }

int rand();

template<typename T>
struct s
{
    int count() { return rand(); }
};

template<typename v>
void f(s<v> a)
{
    int const x = a.count();
    int r = 0;
    auto l = [&](int& r)
    {
        for(int y = 0, yend = (x); y < yend; ++y)
        {
            r += y;
        }
    };
    l(r);
}

template void f(s<float>);
