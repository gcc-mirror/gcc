// { dg-do compile }

struct A { int i;  A();  A(const A&); };

void bar()
{
    A a;
    for ( ;; a=A() ) ;
}
