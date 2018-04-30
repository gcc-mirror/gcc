// { dg-lto-do assemble }

struct Foo {
 static const int dummy;

 int bit_field:1;
 int dummy2:1;
 int dummy3:1;
};

struct Bar {
 Foo foo;
};

int func(const Bar& b) {
 return b.foo.bit_field;
}

struct Baz {
 Bar& operator*() { static Bar a; return a; }
};

void func1(Baz baz, int i, Bar bar) {
 i || func(bar);
 *baz = bar;
}

void func2(Baz baz, Bar bar) {
 func1(baz, 0, bar);
}
