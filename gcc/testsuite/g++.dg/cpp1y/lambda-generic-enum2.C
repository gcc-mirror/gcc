// PR c++/105398
// { dg-do compile { target c++14 } }

auto f = [](auto &&m) {
    enum E { _,e3,e2,e1,C4,C3,C2,C1 };
    static constexpr int x_coeffs[3][4] = {
        {e1,C2,C3,C4},
        {e2,C1,C3,C4},
        {e3,C1,C2,C4},
    };
};

int main() {
    f(0);
}
