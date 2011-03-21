// PR c++/47851
// { dg-options -std=c++0x }

struct Type {
  void display_type();
  void display_type() const { }
};

typedef Type const ConstType;

struct ConvertibleToType {
    operator Type&() { return *reinterpret_cast<Type*>(this); }
};

int main ()
{
  // Both lines should call the const variant.
  (true ? ConvertibleToType() : ConstType()).display_type();
  decltype((true ? ConvertibleToType() : ConstType()))().display_type();
}
