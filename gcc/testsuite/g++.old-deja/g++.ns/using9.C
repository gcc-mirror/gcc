// Test for proper merging of functions from multiple using directives.

// Build don't link:

namespace standard 
{ void print(int) {};
  void dump(int)  {};
}
namespace A { using standard::print; }
namespace B { using namespace standard; }
namespace User
{ using namespace standard; 
  using namespace A;
  void test()
  {  print(1); }
  // egcs-1.1: call of overloaded `print (int)' is ambiguous
}
namespace User2
{ using namespace standard;
  using namespace B;
  void test()
  { print(1); } // egcs has no problems here
}

