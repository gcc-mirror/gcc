// PR c++/48089
// { dg-options -std=c++0x }

struct s {
    constexpr s() : v(v) { }	// { dg-error "object being constructed" }
    char v;
};

s bang;
