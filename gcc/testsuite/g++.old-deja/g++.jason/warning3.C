// Bug: overloading of 'A' for template causes bogus shadowing warnings.
// Special g++ Options: -Wshadow
// Build don't link:

template<class T> 
class A
{
 public:
   virtual ~A() {}
};

template class A<int>;
