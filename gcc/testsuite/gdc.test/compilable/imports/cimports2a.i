extern int xx;

typedef struct Foo *FooRef;
FooRef make_foo(void);


typedef struct Foo2 *FooRef2;
struct Foo2 {
    int x;
};
FooRef2 make_foo2(void);
