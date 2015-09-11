// PR c++/66001
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct dt
    { dt() {} ~ dt() {} };

struct x {
    std::initializer_list< dt > f = { {} };
} cx;

struct x2 {
    struct dt { ~ dt() {} }
        const & m = {};
} cx2;
