// PR c++/48994

// { dg-do compile }
// { dg-options "-std=c++0x" }

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
