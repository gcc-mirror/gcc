// { dg-do assemble  }
// Bug: g++ only looks in the current temporary binding level for a name.

struct T { ~T(); };

int main()
{
  foo:
   T t;				// { dg-error "" } redeclared
  bar:
   T t;				// { dg-error "" } redeclaration
}
