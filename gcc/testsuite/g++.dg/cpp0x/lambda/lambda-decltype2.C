// PR c++/65727
// { dg-do compile { target c++11 } }

struct type_a { void(*cb)(); };

struct type_b
{
    type_b(type_a p);
    void dummy();
};

template<class T>
constexpr T function_c(T**t) {return **t;}

class type_d {
    public:
        static void dummy();
};
class type_e {
    public:
        static type_b b;
        type_d *d[1];
};

type_b type_e::b = {{[](){decltype(function_c(type_e::d))::dummy();}}};
