// PRMS Id: 5656
// Bug: g++ tries (which is a bug) and fails (which is a bug) to initialize
// var at runtime.
// Build don't link:

struct A
{
    int func(int);
    int func() const;
};
int (A::* var) () const = & A::func;
