// { dg-do assemble  }
namespace NS1
{
  int a;
}

namespace NS2 = NonExistant; //{ dg-error "" } 

