// PR c++/53421

template< class T >
struct Y : T
{ } __attribute__((__may_alias__));

struct X
{
  operator Y<X>& () { return *static_cast< Y<X>* >(this); }
};

int main()
{
  &X::operator Y<X>&;
}
