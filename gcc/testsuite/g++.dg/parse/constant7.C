// PR c++/19991
 
enum { e = 1 };

template<typename> struct A
{
  static const int i = e;
  char a[i];
};
