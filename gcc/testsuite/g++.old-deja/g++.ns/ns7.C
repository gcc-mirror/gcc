// Build don't link: 
namespace A{
 struct X{
   int i;
   X(){}
   X(int j);
   void operator=(const X&);
   virtual ~X(){}
 };
 void X::operator=(const X&o)
 {
   i=o.i;
 }
}

A::X::X(int j):i(j){}

namespace A{
  struct Y:public X{
    int j;
    Y(int,int);
  };
}

A::Y::Y(int a,int b):X(a),j(b)
{}
