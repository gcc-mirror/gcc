/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing" } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */

#include <string>
#include <list>

class A;

class B {
public:
    void foo(A&);
    std::string s;
};

class A {
public:
    A& qaz() {
	l.push_back( new A() );
	return *l.back();
    }
    std::list<A*> l;
};

void bar()
{
  A a;
  B b;
  b.foo(a.qaz());
}

