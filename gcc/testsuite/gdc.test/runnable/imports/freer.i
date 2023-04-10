typedef struct Foo *FooRef;
struct Foo {
    int x;
};
void free_foo(FooRef foo) { }
