namespace foo{
 int eine_funktion(int)
 {
   return 0;
 }
}

namespace foo{
 void eine_funktion(int,int)
 {}
}

namespace bar = foo;

int main()
{
  return bar::eine_funktion(3);
}
