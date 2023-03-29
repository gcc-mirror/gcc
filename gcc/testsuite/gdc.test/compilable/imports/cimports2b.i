extern int xx;

typedef struct Foo *FooRef;
void free_foo(FooRef foo);

/****************************/

typedef struct Foo2 *FooRef2;
struct Foo2 {
    int x;
};
void free_foo2(FooRef2 foo);
