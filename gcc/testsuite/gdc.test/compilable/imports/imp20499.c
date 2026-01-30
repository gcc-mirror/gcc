// https://github.com/dlang/dmd/issues/20499
typedef struct Foo {
    int x;
} *pFoo;

struct Bar {
    struct Foo foo;
};
