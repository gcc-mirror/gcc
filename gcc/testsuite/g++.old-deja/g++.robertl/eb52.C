// Build don't link: 
class base {
protected:
    virtual void f();
};

class d1 : public virtual base {
protected:
    void f();
};

void d1::f()
{
    base::f();
}

class dd1 : public virtual d1 {
protected:
     void f();
};

void dd1::f()
{
    d1::f();
    base::f();
}

class d1_and_base : public virtual d1, public virtual base {
protected:
     void f();
};

void d1_and_base::f()
{
    d1::f();
    base::f();
}
