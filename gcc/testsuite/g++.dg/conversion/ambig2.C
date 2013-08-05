// PR c++/57948

struct Base {   };
struct Derived : Base
{
  struct Derived2 : Base
  {
    struct ConvertibleToBothDerivedRef
    {
      operator Derived&();
      operator Derived2&();
      void bind_lvalue_to_conv_lvalue_ambig(ConvertibleToBothDerivedRef both)
      {
	Base &br1 = both; // { dg-error "ambiguous" }
      }
    };
  };
};
