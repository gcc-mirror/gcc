// PR c++/16851

struct A { A(int); };

void f()
{
 throw (3,A(t));
}
