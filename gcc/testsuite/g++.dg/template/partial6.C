// PR c++/41703
// The second GetAllSize template is more specialized because even though
// deduction on each parameter type succeeds, we never get a template
// argument for its X to make it match the first template.

template <typename T, int (T::*)() const>
struct TSizeEnabler
{
    typedef T TClass;
};

template <typename X>
int
GetAllSize(const X &Var)
{ return sizeof(Var); }

template <typename X>
int
GetAllSize(const typename TSizeEnabler<X, &X::func>::TClass &Var)
{ return Var.func(); }

struct H
{
    int func() const;
};

int main()
{
    H b;
    return GetAllSize< H >(b);
}
