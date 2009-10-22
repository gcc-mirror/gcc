// { dg-lto-do link }
// { dg-lto-options {{-O3 -flto -Winline}} }

#include <string>

int
main()
{
  std::string i;
  i = "abc";
}

