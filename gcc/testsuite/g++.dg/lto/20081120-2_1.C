template < typename > struct Foo
{
 inline void rdstate() {
 }
};

extern template struct Foo<int>;

struct Bar:virtual public Foo<int>
{
};
