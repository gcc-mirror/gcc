class Mutable
{
public:
    virtual ~Mutable();
    virtual void func();
};

Mutable::~Mutable()
{
}

class DeriveMutable final : public Mutable
{
public:
    virtual ~DeriveMutable();
    void func() override;
};

DeriveMutable::~DeriveMutable()
{
}

class Const
{
public:
    virtual ~Const();
    virtual void func() const;
};

Const::~Const()
{
}

class DeriveConst final : public Const
{
public:
    virtual ~DeriveConst();
    void func() const override;
};

DeriveConst::~DeriveConst()
{
}

void test23135()
{
    DeriveMutable mut;
    mut.func();

    DeriveConst cst;
    cst.func();
}
