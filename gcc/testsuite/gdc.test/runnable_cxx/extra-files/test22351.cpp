#include <assert.h>

class A22351
{
public:
    virtual int f();
    virtual int g(int *);
    virtual int h();
    virtual int h() const;
};

class B22351 : public A22351
{
public:
    virtual int f() const;
    virtual int g(const int *);
    int h() const override;
};

B22351 *createB();

int main()
{
    // mutable A calls functions in A vtable
    A22351 *a = createB();
    assert(a->f() == 1);
    assert(a->g(0) == 3);
    assert(a->h() == 5);

    // cast to B calls functions in B vtable
    B22351 *b = (B22351 *)a;
    assert(b->f() == 2);
    assert(b->g(0) == 4);
    assert(b->h() == 6);

    // cast to const calls B override function
    const A22351 *ca = a;
    assert(ca->h() == 6);

    // const B calls functions in B vtable
    const B22351 *cb = createB();
    assert(cb->f() == 2);
    assert(cb->h() == 6);

    return 0;
}
