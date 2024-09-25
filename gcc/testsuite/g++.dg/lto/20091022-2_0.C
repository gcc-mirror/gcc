// { dg-lto-do link }
// { dg-lto-options {{-O3 -flto -Winline}} }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

int
main()
{
  std::string i;
  i = "abc";
}

