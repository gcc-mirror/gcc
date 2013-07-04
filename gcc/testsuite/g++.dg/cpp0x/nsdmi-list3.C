// PR c++/54998
// { dg-do compile { target c++11 } }

class Foo {
public:
private:
    static const int kRows = 4;
    static const int kCols = 4;

    union {
        float m_n[kRows][kCols];
        float m_m[kRows * kCols] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
        };
    };
};

Foo myFoo;
