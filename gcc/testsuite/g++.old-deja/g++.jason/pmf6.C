// { dg-do assemble  }
// PRMS Id: 5656
// Bug: g++ tries (which is a bug) and fails (which is a bug) to initialize
// var at runtime.

struct A
{
    int func(int);
    int func() const;
};
int (A::* var) () const = & A::func;
