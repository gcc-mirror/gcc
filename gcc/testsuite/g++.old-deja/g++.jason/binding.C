// Bug: g++ only looks in the current temporary binding level for a name.

struct T { ~T(); };

int main()
{
  foo:
   T t;				// ERROR - redeclared
  bar:
   T t;				// ERROR - redeclaration
}
