// Build don't link: 
namespace foo{
 void eine_funktion(int)
 {}
}

using namespace foo;

namespace foo{
 void eine_funktion(int,int)
 {}
}

void andere_funktion()
{
  eine_funktion(3,4);
}
