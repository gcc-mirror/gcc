        #include <iostream.h>
        struct foo {
                foo(int x) { cerr << "foo's int constructor (" << x << ")\n"; };
        };
        struct bar : foo {
                typedef int an_int;
                bar() : bar::an_int(3) {};  // will call foo::foo(3)
        };
int main() { bar b; }
