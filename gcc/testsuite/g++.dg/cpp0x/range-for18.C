// PR c++/48994

// { dg-do compile { target c++11 } }

template <typename T>
struct myvec
{
    T* begin() const;
    T* end() const;
};

void f(const myvec<int>& v)
{
    for (int i : v)
        ;
}
