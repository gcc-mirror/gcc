// { dg-do run  }
namespace foo{
 struct X{
   int i;
   void f();
   static int k1,k2;
 };
 void X::f(){}
 int var;
 int X::k1;
}

using namespace foo;
X zzz;
int X::k2;

void andere_funktion()
{
  zzz.f();
  var=4;
}

int main(int,char**)
{
  andere_funktion();
  return 0;
}
