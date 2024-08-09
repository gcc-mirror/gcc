// PR c++/116015
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-c++20-extensions" }

struct MatrixLayout {
    int rows         = 0;
    int outer_stride = rows;
};
struct Matrix {
    Matrix(MatrixLayout m) {}
};
struct Widget {
    int n = 5;
    Matrix A0{{}};
    Matrix A1{{n}};
    Matrix A1_{{.rows = n}};
    Matrix A2{{n, n}};
};

int
main ()
{
 Widget w{};
 Widget w1;
 Widget w2 = {};
}
