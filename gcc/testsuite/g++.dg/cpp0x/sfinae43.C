// PR c++/56208
// { dg-options -std=c++11 }

struct ostream {
  ostream& operator<<(int);
};

struct sfinae_base {

  typedef char one;
  typedef char (&two)[2];

  template<class T>
  static T make();

  template<unsigned> struct ok { typedef int type; };

  template<class U, class T>
  static one test(decltype((make<U>() << make<T>()), 0));

  template<class, class>
  static two test(...);
};

template<class T>
struct is_printable : private sfinae_base
{
  enum { value = sizeof(test<ostream&, T>(0)) == sizeof(one) };
};

typedef int ok[is_printable<int>::value ? 1 : -1];
