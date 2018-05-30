// { dg-lto-do link }
// { dg-lto-options {{-flto -r -nostdlib}} }
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
template < typename > struct Foo
{
 inline void rdstate() {
 }
};

extern template struct Foo<int>;

struct Bar:virtual public Foo<int>
{
};
