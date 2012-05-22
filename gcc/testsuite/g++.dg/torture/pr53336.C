// { dg-do compile }

bool foo();

struct C
{
    C()
    {
        if (foo())
            foo();
    }
};

struct S
{
    struct dummy
    {
        int i_;
    };
    typedef int dummy::*bool_type;

    operator bool_type() const
    {
        return foo() ? &dummy::i_ : 0;
    }
};

int x;

struct adaptor
{
    C c;

    virtual void bar()
    {
        if (S())
            x = 0;
    }
};

int main()
{
    adaptor a;
}

