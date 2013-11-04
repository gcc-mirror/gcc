// PR c++/46056
// Check that range-based for loop calls destructors 
// when required
// { dg-options "-std=c++11" }
// { dg-do run }
extern "C" void abort();

int value_counter = 0, it_counter = 0, seq_counter = 0;

struct Int
{
    int x;
    Int(int v)
        :x(v)
    {
        ++value_counter;
    }
    Int(const Int &o)
        :x(o.x)
    {
        ++value_counter;
    }
    ~Int()
    {
        --value_counter;
    }
};

struct iterator
{
    int x;
    iterator(int v) 
        :x(v) 
    {
        ++it_counter;
    }
    iterator(const iterator &o)
        :x(o.x)
    {
        ++it_counter;
    }
    ~iterator() 
    {
        --it_counter;
    }
    iterator &operator ++() { ++x; return *this; }
    int operator *() { return x; }
    bool operator != (const iterator &o) { return x != o.x; }
};

struct container
{
    int min, max;
    container(int a, int b) :min(a), max(b)
    {
        ++seq_counter;
    }
    container(const container &) = delete;
    ~container()
    {
        --seq_counter;
    }
};

iterator begin(container &c)
{
    return iterator(c.min);
}

iterator end(container &c)
{
    return iterator(c.max + 1);
}

int main()
{
    for (Int x : container(0, 10))
    {
        if (value_counter != 1) abort();
        if (it_counter != 2) abort();
        if (seq_counter != 1) abort();
    }
    if (value_counter != 0) abort();
    if (it_counter != 0) abort();
    if (seq_counter != 0) abort();

    try
    {
        for (Int x : container(0, 10))
        {
            if (value_counter != 1) abort();
            if (it_counter != 2) abort();
            if (seq_counter != 1) abort();
        }
        if (value_counter != 0) abort();
        if (it_counter != 0) abort();
        if (seq_counter != 0) abort();

        for (Int x : container(0, 10))
        {
            if (value_counter != 1) abort();
            if (it_counter != 2) abort();
            if (seq_counter != 1) abort();

            if (x.x == 5)
                throw 0;
        }
    }
    catch (int)
    {
        if (value_counter != 0) abort();
        if (it_counter != 0) abort();
        if (seq_counter != 0) abort();
    }

    return 0;
}
