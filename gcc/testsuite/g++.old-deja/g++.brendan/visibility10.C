// Build don't link: 
// GROUPS passed visibility
struct base
{
    protected:
        void base_func() {}// ERROR - .*is protected.*
};

struct derived : public base
{
    protected:
        void derived_func(base *ptr) { ptr->base_func(); }// ERROR -  within this context
};

