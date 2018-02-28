// PR c++/83806
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-parameter" }

template <class X, class Y>
bool equals(X x, Y y) {
    return (x == y); 
}

int main() {
    const char* p = nullptr;
    equals(p, nullptr);
}
