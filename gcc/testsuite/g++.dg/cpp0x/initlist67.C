// PR c++/50478
// { dg-do compile { target c++11 } }

#include <initializer_list>

namespace std
{
  template<typename _Key>
    struct set
    {
      void insert(const _Key&);
      void insert(initializer_list<_Key>);
    };

  struct string
  {
    string(const string&, __SIZE_TYPE__, __SIZE_TYPE__ = -1);
    string(const char*);
    string(initializer_list<char>);
  };
}

int main()
{
  std::set<std::string> s;
  s.insert( { "abc", "def", "hij"} );
}
