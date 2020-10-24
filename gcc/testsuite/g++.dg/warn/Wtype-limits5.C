// PR c++/96742
// { dg-additional-options "-Wtype-limits" }

template <unsigned N>
bool f(unsigned x) {
    return unsigned(x < N);
}

int main() {
    f<0>(1);
}
