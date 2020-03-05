// PR c++/93442
// { dg-do compile { target c++17 } }

struct bar {
    int foo(){return 0;}
};

int foobar() {
    if constexpr(true) {
        return 0;
    } else {
        return [](){
            return bar{};
        }().foo();
    }
}
