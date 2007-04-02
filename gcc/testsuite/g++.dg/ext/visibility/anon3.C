// PR c++/31187
// Bug: the repeated declaration was confusing the compiler into
// thinking that foo1 had language internal linkage.

class foo { };

namespace
{
  extern foo foo1;
  foo foo1;
}

template< foo * >
class bar { };

bar< &foo1 > bar1;
