// Build don't link: 
// GROUPS passed nest
struct B
{
    class B_I { };
};

struct D : public B
{
    B_I foo;
    class I : public B_I { };
};

