// { dg-do compile { target c++11 } }

class Foo
{
public:
  Foo() { }
  int operator"" _Bar(char32_t);  // { dg-error "7:.int Foo::operator\"\"_Bar\\(char32_t\\). must be a non-member function" }
};

int i = operator"" _Bar(U'x');  // { dg-error "9:'operator\"\"_Bar' was not declared in this scope" }
int j = U'x'_Bar;  // { dg-error "unable to find character literal operator" }

int
Foo::operator"" _Bar(char32_t)  // { dg-error "must be a non-member function" }
{ return 42; }
