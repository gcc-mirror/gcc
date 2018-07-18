// PR c++/78124
// { dg-do compile { target c++11 } }

struct base {
    explicit constexpr base(int&&) {}
};

struct derived: base {
    using base::base;
};

int main()
{
    derived d { 0 };
}

