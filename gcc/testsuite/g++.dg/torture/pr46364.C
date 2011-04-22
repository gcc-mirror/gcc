// { dg-do compile }
#include <string>

void a() throw (int);
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
