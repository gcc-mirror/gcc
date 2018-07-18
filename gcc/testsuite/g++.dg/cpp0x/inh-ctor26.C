// PR c++/79503
// { dg-do compile { target c++11 } }

struct payload {};

struct base: private payload {
    base(payload) {}
};

struct derived: base {
    using base::base;
};

int main()
{
    payload data;
    // error: no matching function for call to 'derived::derived(payload&)'
    // note: candidate: base::base(payload)
    // note:   an inherited constructor is not a candidate for initialization from an expression of the same or derived type
    derived demo(data);
}
