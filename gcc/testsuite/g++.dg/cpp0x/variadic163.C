// PR c++/63786
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <int... Is>
int f(int i) {
    switch (i) {
        case Is:       // { dg-error "not expanded" }
            return 0;
    }

    switch (i) {
        case 0 ...Is:  // { dg-error "not expanded" }
            return 0;
    }
    return 0;
}

int main() {
    f<1,2,3>(1);
}
