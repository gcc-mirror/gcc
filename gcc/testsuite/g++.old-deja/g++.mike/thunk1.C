// { dg-do assemble  }

struct C1
{
    virtual ~C1();
};

struct C2 : public virtual C1
{
        virtual ~C2();
};

struct C3 : public virtual C2
{
    virtual ~C3();
};

C3::~C3() {}
