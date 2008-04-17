// PR c++/35773

template< typename T >
class auto_ptr
{
  struct auto_ptr_ref { };
public:
  auto_ptr(auto_ptr&);
  auto_ptr(auto_ptr_ref);

  operator auto_ptr_ref();
};

template< typename T >
class reference_wrapper
{
public:
  reference_wrapper(T& t);
  operator T& () const;
};

struct X { };

void f(auto_ptr< X >);

void g(reference_wrapper< auto_ptr< X > > r)
{
  f(r);
}
