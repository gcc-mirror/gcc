// { dg-do assemble  }

typedef	unsigned int atypedef;
struct astruct{};
void afunction();
void aovlfunction();
void aovlfunction(int);
int avariable;

namespace foo {
  using ::atypedef;
  using ::astruct;
  using ::afunction;
  using ::aovlfunction;
  using ::avariable;
}
