// Build don't link:
class A {
public:
    void malloc(unsigned int);
};

void A::malloc(unsigned int) {}

void foo() {
    A a;
    a.malloc(3);    // <-- line 10
}
