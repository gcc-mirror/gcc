// PR c++/37558

class Foo {
 friend class Bar;
 friend void func(const class Bar*);
};
