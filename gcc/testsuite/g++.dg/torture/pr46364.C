// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
#include <string>

void a()
#if __cplusplus <= 201402L
throw (int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
;
void b(std::string const &);

void c(std::string *e)
{
  b("");

  try {
      a();
  } catch (...) {
      *e = "";
  }
}

void d() {
    c(0);
}
