// PR c++/16851

struct A { A(int); };

void f(int t)
{
 throw (3,A(t));
}
