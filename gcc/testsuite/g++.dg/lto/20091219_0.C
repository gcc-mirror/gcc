// { dg-lto-do run }
// { dg-lto-options {{-O3 -flto}} }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>
#include <map>

int main ()
{
  typedef std::map<int, std::string> Map;
  static Map m;

  Map::const_iterator it = m.find(0);
  if (it != m.end())
    std::string s = it->second;

  return 0;
}
