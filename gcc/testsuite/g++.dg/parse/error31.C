// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/32190

template<typename T> class foo{ };

int main() {
    foo<int> i;
    // this column number is not accurate yet, but that will make it for now.
    foo<foo<int> j; // { dg-error "18:template argument 1 is invalid" }
    int k;
    int l;
    foo<int> m;
    return 0;
    }
