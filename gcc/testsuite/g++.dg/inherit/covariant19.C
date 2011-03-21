// PR c++/46220
// According to the letter of the standard this is invalid,
// but that seems like a bug.

class Baz;
class Foo {
public:
    virtual const Baz* getBaz() = 0;
};
class Bar : public Foo {
public:
    Baz* getBaz();
};

