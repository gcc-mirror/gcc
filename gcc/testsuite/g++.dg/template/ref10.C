// PR c++/80840
// { dg-do compile { target c++11 } }

template <class T, T X>
struct Just;

template <const double& X>
struct Number {
    static constexpr double value = X;
    using result = Just<const double&, value>;
};

int main() {}
