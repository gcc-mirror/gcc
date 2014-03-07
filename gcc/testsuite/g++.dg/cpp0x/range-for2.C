// Test for range-based for loop
// Test the loop with a custom iterator
// with begin/end in an associated namespace

// { dg-do compile { target c++11 } }

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
    };

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
    foo::container c(1,4);
    for (int it : c)
        ;
}
