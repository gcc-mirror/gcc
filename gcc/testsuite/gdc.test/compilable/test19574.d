
static assert( Foo(10).bar.value == 10 );

extern(C++, "ns") {
    struct Foo {
        Bar!Foo bar;

        this( int v ) {
            bar.value = v;
        }
    }
}

extern(C++, "ns") {
    struct Bar(T) {
        int value;
    }
}
