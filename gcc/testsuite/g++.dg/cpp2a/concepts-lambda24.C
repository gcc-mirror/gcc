// PR c++/120123
// { dg-do compile { target c++20 } }

struct H {
    void member(int) {}
    void call() {
        [this]() {
            [this](const auto& v)
                requires requires { /*this->*/member(v); }
            { return member(v); }(0);
        };
    }
};
