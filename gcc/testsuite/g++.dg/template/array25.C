// PR c++/55249

template <typename _Tp> struct A
{
  _Tp _M_instance[1];
};
template <class> struct inner_type
{
    inner_type () {}
    inner_type (inner_type &);
    inner_type (const inner_type &) {}
};

int
main ()
{
    A <inner_type <int> > a, b = a;
}
