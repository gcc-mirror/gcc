// Test for range-based for loop
// Test the loop with a custom iterator
// with begin/end in std

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

struct container
{
    int min, max;
    container(int a, int b) :min(a), max(b) {}
};

namespace std
{
    iterator begin(container &c)
    {
        return iterator(c.min);
    }

    iterator end(container &c)
    {
        return iterator(c.max + 1);
    }
}

int main()
{
    container c(1,4);
    for (int it : c)    // { dg-error "was not declared" }
    {
    }
}
