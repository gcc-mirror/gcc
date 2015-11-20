// { dg-do compile { target c++11 } }

constexpr int f(void *) { return 0; }
constexpr int f(...) { return 1; }
constexpr int g1() { return f(0); }
constexpr int g2(int n) { return f(n); }
constexpr int g3(int n) { return f(n*0); }

int main()
{
    static_assert(g1() == 0, "g1 failed");
    static_assert(g2(0) == 1, "g2 failed");
    static_assert(g3(0) == 1, "g3 failed");
}
