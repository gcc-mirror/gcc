// { dg-do assemble  }
// GROUPS passed visibility
struct base
{
    protected:
        void base_func() {}// { dg-error "" } .*is protected.*
};

struct derived : public base
{
    protected:
        void derived_func(base *ptr) { ptr->base_func(); }// { dg-error "" }  within this context
};

