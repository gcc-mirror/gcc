// PR c++/81952
// { dg-do run { target c++11 } }

template <class T>
struct opt {
    opt() { }
    opt(opt const& ) { __builtin_abort (); }
    opt(opt&& ) { __builtin_abort (); }

    template <class U>
    opt(U&& ) { }
};

struct foo 
{
    explicit operator opt<int>() { __builtin_abort (); return {}; }
};

int main()
{
    opt<int> o(foo{});
}
