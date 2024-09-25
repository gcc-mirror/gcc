// PR c++/64665, DR 1467 
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

bool Test1(bool);
bool Test1(std::string) = delete;

bool Test2(int) = delete;
bool Test2(std::initializer_list<int>);

struct S 
{ 
    S(int _a) : a(_a) {}
private:
    int a;
};
bool Test3(int);
bool Test3(S) = delete;

bool Test4(bool) = delete;
bool Test4(std::initializer_list<std::string>);

int main () 
{
  ( Test1({"false"}) );	// { dg-error "narrowing" }
  ( Test2({123}) );
  ( Test3({456}) );
  ( Test4({"false"}) );
}
