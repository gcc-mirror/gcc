// PR c++/32190

template<typename T> class foo{ };

int main() {
    foo<int> i;
    foo<foo<int> j; // { dg-error "template argument" }
    int k;
    int l;
    foo<int> m;
    return 0;
    }
