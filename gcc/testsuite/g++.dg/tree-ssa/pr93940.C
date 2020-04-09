/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-Og --coverage -pthread -fdump-tree-optimized -std=c++17" } */
using uint16_t = unsigned short;

struct a {
    uint16_t b = 0;
};
struct c {
    short d;
};
class e {
public:
    void f();
    void init_session(c);
};

auto htons = [](uint16_t s) {
    if (__builtin_constant_p(s)) {
        return uint16_t(uint16_t(s >> 8) | uint16_t(s << 8));
    }
    return uint16_t(uint16_t(s >> 8) | uint16_t(s << 8));
};

struct g {
    e h;
    void i(a k) {
        h.f();
        auto j = c();
        j.d = htons(k.b);
        h.init_session(j);
    }
};

void test() {
    g().i({});
}

/* { dg-final { scan-tree-dump-not "builtin_unreachable" "optimized"} } */
