// PR c++/38950

template <typename T, T N> void f(T(&)[N]);

int main() {
    int x[2];
    unsigned int y[2];
    f(x); // works
    f(y); // ICE
}
