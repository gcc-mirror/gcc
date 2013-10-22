// Test for range-based for loop
// Test the loop with a custom iterator
// with begin/end as member functions

// { dg-do compile }
// { dg-options "-std=c++11" }

struct iterator
{
    int x;
    explicit iterator(int v) :x(v) {}
    iterator &operator ++() { ++x; return *this; }
    int operator *() { return x; }
    bool operator != (const iterator &o) { return x != o.x; }
};

namespace foo
{
    struct container
    {
        int min, max;
        container(int a, int b) :min(a), max(b) {}

        iterator begin()
        {
            return iterator(min);
        }
        iterator end()
        {
            return iterator(max + 1);
        }
    };
}

int main()
{
    foo::container c(1,4);
    for (int it : c)
        ;
}
