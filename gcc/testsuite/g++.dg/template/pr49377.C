// PR c++/49377

template <typename T, int N> class v;

template <typename T> class v<T,2> {
    v() { };
} __attribute__((__may_alias__));

typedef v<float,2> float2;

class a {
    void f(float2 &point);
    float2 d;
};

void a::f(float2 &point) { }
