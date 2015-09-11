// PR c++/65974
// { dg-options "-Wdeprecated" }

struct S {
    void bar();

    __attribute__((deprecated("use bar() instead.")))
    virtual void foo();
};

void S::foo() { bar(); }

int main()
{
    return 0;
}
