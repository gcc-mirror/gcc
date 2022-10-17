// PR c++/101030
// { dg-do compile { target { c++11 } } }
// { dg-options "-Wconversion" }

template <int N>
struct jj {
    int ii[N ?: 1];
    char c = N ?: 1; // { dg-warning "conversion from .int. to .char. changes value from .300. to " }
};

int main() {
    jj<300> kk;
}
