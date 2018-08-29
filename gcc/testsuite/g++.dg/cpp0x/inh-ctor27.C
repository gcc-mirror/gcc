// PR c++/70844
// { dg-options -Wuseless-cast }
// { dg-do compile { target c++11 } }

struct base {
    base (int const &);
};

struct derived : public base {
    using base::base;
};

derived d(0);
